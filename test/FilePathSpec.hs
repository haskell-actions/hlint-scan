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
-- Description: Tests for the "FilePath" module.
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
module FilePathSpec (spec) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (isPrefixOf)
import Data.String (fromString)
import FilePath
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "normalize" $ do
    prop "does not change strings which are not URIs" $ \v ->
      not (hasURI v) ==> normalize v `shouldBe` v

    prop "normalizes relative URIs" $ \n ->
      forAll (arbitrary `suchThat` (not . isPrefixOf "./")) $ \relativeUri ->
        let prefix = take (2 * n) $ cycle "./"
            uri = prefix <> relativeUri
            toObject s =
              Object $
                KeyMap.singleton "artifactLocation" $
                  Object $
                    KeyMap.singleton "uri" $
                      String s
            value = toObject $ fromString uri
            value' = toObject $ fromString relativeUri
         in counterexample (show value) $
              conjoin
                [ hasURI value `shouldBe` True,
                  normalize value `shouldBe` value'
                ]

hasURI :: Value -> Bool
hasURI (Object v)
  | KeyMap.member "uri" v = True
  | otherwise = foldr ((||) . hasURI) False v
hasURI (Array vs) = or $ fmap hasURI vs
hasURI _ = False
