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
-- Description: Tests for the "Arguments" module.
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
module ArgumentsSpec (spec) where

import Arguments
import Data.Maybe (isJust, isNothing)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "validate" $ do
    it "validates empty list" $
      validate [] `shouldBe` Nothing

    it "validates good arguments" $
      validate
        [ "binary=/hlint",
          "path=.",
          "hints=.hlint.yaml",
          "category=code-quality",
          "token=AB12CD"
        ]
        `shouldBe` Nothing

    prop "argument must have '=' character" $ \s ->
      '=' `notElem` s ==>
        validate [s] `shouldSatisfy` isJust

    prop "argument must not have duplicate keyword" $ \key v v' ->
      '=' `notElem` key ==> \keyValues ->
        let otherArgs = map (\(x, y) -> x <> "=" <> y) keyValues
            args' = [key <> "=" <> v, key <> "=" <> v'] ++ otherArgs
         in forAll (shuffle args') $ \args ->
              validate args `shouldSatisfy` isJust

    prop "argument must have explicitly allowed keyword" $ \key v ->
      '=' `notElem` key ==>
        key `notElem` ["binary", "path", "hints", "category", "token"] ==>
          validate [key <> "=" <> v]
            `shouldSatisfy` isJust

  describe "translate" $ do
    it "translates specific arguments" $
      translate
        [ "binary=/hlint",
          "path=.",
          "hints=.hlint.yaml",
          "category=code-quality",
          "token=XYZ123"
        ]
        `shouldBe` ( "/hlint",
                     [".", "--hint=.hlint.yaml", "-j", "--sarif", "--no-exit-code"],
                     Just "code-quality",
                     Just "XYZ123"
                   )

    prop "translates missing category to Nothing" $
      translate [] `shouldSatisfy` \(_, _, category, _) -> isNothing category

    prop "translates missing token to Nothing" $
      translate [] `shouldSatisfy` \(_, _, _, token) -> isNothing token

    prop "translates empty binary to default binary" $
      translate ["binary="]
        `shouldSatisfy` \(binary, _, _, _) -> binary == "/hlint"

    prop "translates empty path to default path" $
      translate ["path="]
        `shouldSatisfy` \(_, args, _, _) -> args == [".", "-j", "--sarif", "--no-exit-code"]

    prop "translates empty hints to omitted hints file flag" $
      translate ["hints="]
        `shouldSatisfy` \(_, args, _, _) -> args == [".", "-j", "--sarif", "--no-exit-code"]

    prop "translates empty category to Nothing" $
      translate ["category="]
        `shouldSatisfy` \(_, _, category, _) -> isNothing category

    prop "translates empty token to Nothing" $
      translate ["token="]
        `shouldSatisfy` \(_, _, token, _) -> isNothing token

    prop "translates general arguments" $ \binary path hints category token ->
      binary /= "" && path /= "" && hints /= "" && category /= "" && token /= ""
        ==> forAll
          ( shuffle
              [ "binary=" <> binary,
                "path=" <> path,
                "hints=" <> hints,
                "category=" <> category,
                "token=" <> token
              ]
          )
        $ \args ->
          translate args
            `shouldBe` ( binary,
                         [path, "--hint=" <> hints, "-j", "--sarif", "--no-exit-code"],
                         Just category,
                         Just token
                       )
