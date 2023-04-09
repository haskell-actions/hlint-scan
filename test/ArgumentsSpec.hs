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
import Data.Maybe (isJust)
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
        key `notElem` ["binary", "path", "category", "token"] ==>
          validate [key <> "=" <> v] `shouldSatisfy` isJust
