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
-- Description: Tests for the "Format" module.
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
module FormatSpec (spec) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Format
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()

spec :: Spec
spec = do
  describe "formatMessages" $ do
    it "formats a particular message" $
      formatMessages
        ( objectWithMessage $
            Text.unlines
              [ "Bad.hs:16:9-20: Warning: Use /=",
                "Found:",
                "  not (a == b)",
                "Perhaps:",
                "  a /= b",
                "Note: incorrect if either value is NaN"
              ]
        )
        `shouldBe` objectWithMessage
          ( Text.unlines
              [ "Bad.hs:16:9-20:&nbsp;Warning:&nbsp;Use&nbsp;/=",
                "&nbsp;&nbsp;",
                "Found:",
                "&nbsp;&nbsp;not&nbsp;(a&nbsp;==&nbsp;b)",
                "&nbsp;&nbsp;",
                "Perhaps:",
                "&nbsp;&nbsp;a&nbsp;/=&nbsp;b",
                "&nbsp;&nbsp;",
                "Note:&nbsp;incorrect&nbsp;if&nbsp;either&nbsp;value&nbsp;is&nbsp;NaN"
              ]
          )

    prop "formats messages in general" $
      forAll (listOf chooseSection) $ \sections ->
        let message = mconcat sections
            message' =
              Text.replace "\\" "&bsol;" $
                Text.replace " " "&nbsp;" $
                  Text.intercalate "  \n" sections
         in counterexample (show message) $
              counterexample (show message') $
                formatMessages (objectWithMessage message) `shouldBe` objectWithMessage message'

objectWithMessage :: Text -> Value
objectWithMessage message =
  Object . KeyMap.singleton "runs" $
    Array . Vector.singleton . Object . KeyMap.singleton "results" $
      Array . Vector.singleton . Object . KeyMap.singleton "message" $
        Object . KeyMap.singleton "text" $
          String message

chooseSection :: Gen Text
chooseSection = do
  note <- arbitrary `suchThat` isNonEmptyLine
  codelines <- map ("  " <>) <$> arbitrary `suchThat` all isNonEmptyLine
  return $ Text.unlines $ note : codelines
  where
    isNonEmptyLine s =
      not
        ( Text.null s
            || Text.take 1 s == " "
            || Text.elem '\n' s
            || Text.elem '\r' s
            || Text.all isSpace s
        )
        && Text.all (\c -> isAlphaNum c || c == ' ') s
