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
import Data.List (isPrefixOf)
import Data.Maybe (isJust, isNothing)
import SpecialOutput qualified
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
          "token=AB12CD",
          "fail-on="
        ]
        `shouldBe` Nothing

    prop "argument must have '=' character" $ \s ->
      ('=' `notElem` s) ==>
        validate [s] `shouldSatisfy` isJust

    prop "argument must not have duplicate keyword" $ \key v v' ->
      '=' `notElem` key ==> \keyValues ->
        let otherArgs = map (\(x, y) -> x <> "=" <> y) keyValues
            args' = [key <> "=" <> v, key <> "=" <> v'] ++ otherArgs
         in forAll (shuffle args') $ \args ->
              validate args `shouldSatisfy` isJust

    prop "argument must have explicitly allowed keyword" $ \key v ->
      ('=' `notElem` key) ==>
        (key `notElem` ["binary", "path", "hints", "category", "token"]) ==>
          validate [key <> "=" <> v] `shouldSatisfy` isJust

    describe "correct argument values" $ do
      describe "fail-on" $ do
        it "is nothing" $ do
          validate [] `shouldBe` Nothing
          validate ["fail-on="] `shouldBe` Nothing

        it "is never" $
          validate ["fail-on=never"] `shouldBe` Nothing

        it "is error" $
          validate ["fail-on=error"] `shouldBe` Nothing

        it "is warning" $
          validate ["fail-on=warning"] `shouldBe` Nothing

        it "is note" $
          validate ["fail-on=note"] `shouldBe` Nothing

        prop "rejects bad values" $ \v ->
          v `notElem` ["", "never", "error", "warning", "note"] ==>
            validate ["fail-on=" <> v] `shouldSatisfy` isJust

    prop "path may not look like a flag" $ \pathSuffix paths' ->
      forAll (shuffle $ ("-" <> pathSuffix) : paths') $ \paths ->
        validate ["path=" <> unwords paths] `shouldSatisfy` isJust

  describe "translate" $ do
    it "translates specific arguments" $
      translate
        [ "binary=/hlint",
          "path=.",
          "hints=.hlint.yaml",
          "category=code-quality",
          "token=XYZ123",
          "fail-on=warning"
        ]
        `shouldBe` ( "/hlint",
                     [".", "--hint=.hlint.yaml", "-j", "--sarif", "--no-exit-code"],
                     Just "code-quality",
                     Just "XYZ123",
                     Just SpecialOutput.Warning
                   )

    prop "translates missing category to Nothing" $
      translate [] `shouldSatisfy` \(_, _, category, _, _) -> isNothing category

    prop "translates missing token to Nothing" $
      translate [] `shouldSatisfy` \(_, _, _, token, _) -> isNothing token

    prop "translates empty binary to default binary" $
      translate ["binary="]
        `shouldSatisfy` \(binary, _, _, _, _) -> binary == "/hlint"

    prop "translates empty path to default path" $
      translate ["path="]
        `shouldSatisfy` \(_, args, _, _, _) -> args == [".", "-j", "--sarif", "--no-exit-code"]

    prop "translates empty hints to omitted hints file flag" $
      translate ["hints="]
        `shouldSatisfy` \(_, args, _, _, _) -> args == [".", "-j", "--sarif", "--no-exit-code"]

    prop "translates empty category to Nothing" $
      translate ["category="]
        `shouldSatisfy` \(_, _, category, _, _) -> isNothing category

    prop "translates empty token to Nothing" $
      translate ["token="]
        `shouldSatisfy` \(_, _, _, token, _) -> isNothing token

    prop "translates empty fail-on to Nothing" $
      translate ["fail-on="]
        `shouldSatisfy` \(_, _, _, _, failOn) -> isNothing failOn

    let failOn (_, _, _, _, x) = x
     in describe "translates fail-on arguments" $ do
          it "empty string" $ failOn (translate ["fail-on="]) `shouldBe` Nothing
          it "never" $ failOn (translate ["fail-on=never"]) `shouldBe` Just SpecialOutput.Never
          it "error" $ failOn (translate ["fail-on=error"]) `shouldBe` Just SpecialOutput.Error
          it "warning" $ failOn (translate ["fail-on=warning"]) `shouldBe` Just SpecialOutput.Warning
          it "note" $ failOn (translate ["fail-on=note"]) `shouldBe` Just SpecialOutput.Note

    prop "translates general arguments" $ \binary paths hints category token ->
      (binary /= "")
        && not (null $ words paths)
        && not (any null $ words paths)
        && not (any (isPrefixOf "-") $ words paths)
        && (hints /= "")
        && (category /= "")
        && (token /= "")
        ==> forAll
          ( shuffle
              [ "binary=" <> binary,
                "path=" <> paths,
                "hints=" <> hints,
                "category=" <> category,
                "token=" <> token
              ]
          )
        $ \args ->
          translate args
            `shouldBe` ( binary,
                         words paths
                           ++ [ "--hint=" <> hints,
                                "-j",
                                "--sarif",
                                "--no-exit-code"
                              ],
                         Just category,
                         Just token,
                         Nothing
                       )
