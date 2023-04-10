{-
Copyright 2023 Google LLC

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

-- |
-- Description: Tests for the "Upload" module.
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
module UploadSpec (spec) where

import Codec.Compression.GZip (compress)
import Data.Aeson hiding ((.:))
import Data.Aeson.KeyMap
import Data.ByteString.Lazy.Base64 (encodeBase64)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import GitHub.REST
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Upload

spec :: Spec
spec = do
  describe "toCall" $ do
    prop "converts environment and output to call" $ \repo sha ref workspace output ->
      let call =
            toCall
              [ ("GITHUB_REPOSITORY", repo),
                ("GITHUB_SHA", sha),
                ("GITHUB_REF", ref),
                ("GITHUB_WORKSPACE", workspace)
              ]
              output
       in conjoin
            [ method <$> call `shouldBe` Just POST,
              endpoint <$> call `shouldBe` Just "/repos/:repo/code-scanning/sarifs",
              -- KeyValue is not instance of Eq
              show . endpointVals <$> call `shouldBe` Just (show $ ["repo" := repo]),
              extractSARIF . ghData <$> call
                `shouldBe` Just (toStrict $ encodeBase64 $ compress $ output)
            ]

  describe "toSettings" $ do
    prop "has expected user agent" $ \t ->
      userAgent (toSettings t) `shouldBe` "github.com/haskell-actions/hlint-scan"

    prop "has no access token" $
      -- Token is not instance of Eq
      show (token $ toSettings Nothing)
        `shouldBe` "Nothing"

    prop "has no access token" $
      -- Token is not instance of Eq
      show (token $ toSettings $ Just "ABCDE")
        `shouldBe` "Just (AccessToken \"ABCDE\")"

  describe "toOutputs" $ do
    prop "include id" $ \x ->
      toOutputs (Object $ singleton "id" $ String $ fromString x)
        `shouldBe` ["sarif-id=" <> x]

extractSARIF :: GitHubData -> Text
extractSARIF x = (toJSON x) .: "sarif"
