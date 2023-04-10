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
-- Description: Tests for the "Fingerprint" module.
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
module FingerprintSpec (spec) where

import Data.Aeson
import Data.Aeson.KeyMap
import Data.String (fromString)
import Data.Text (Text)
import Data.Vector qualified as Vector
import Fingerprint
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "fill" $ do
    prop "does not overwrite existing partial fingerprint" $ \rule' level' declaration' fp' ->
      let rule = fromString rule'
          level = fromString level'
          declaration = fromString declaration'
          fp = fromString fp'
          logicalLocations =
            Array . Vector.singleton . Object $
              fromList [("fullyQualifiedName", String declaration)]
          locations = Array $ Vector.singleton logicalLocations
          partialFingerprints =
            Array . Vector.singleton . Object $
              singleton "NotLogicalCodeIssue/v2" (String fp)
          result =
            Object . fromList $
              [ ("ruleId", String rule),
                ("level", String level),
                ("locations", locations),
                ("partialFingerprints", partialFingerprints)
              ]
          value =
            Object . singleton "runs" $
              Array . Vector.singleton . Object $
                singleton "results " . Array $
                  Vector.singleton result
       in fill value `shouldBe` value

    -- Confirm that partial fingerprints are filled.
    -- These also check that they are particular values,
    -- since partial fingerprints are supposed to be stable.
    describe "fills with stable partial fingerprints" $ do
      it "\"Use Haskell\"" $
        let value = constructLog ("Use Haskell", "note", "someBadLanguage", Nothing)
            value' = constructLog ("Use Haskell", "note", "someBadLanguage", Just "VXNlIEhhc2tlbGw6bm90ZTo6")
         in fill value `shouldBe` value'

      it "\"Use concatMap\"" $
        let value = constructLog ("Use concatMap", "warning", "Transit.walk", Nothing)
            value' = constructLog ("Use concatMap", "warning", "Transit.walk", Just "VXNlIGNvbmNhdE1hcDp3YXJuaW5nOjo=")
         in fill value `shouldBe` value'

      it "\"Use map once\"" $
        let value = constructLog ("Use map once", "warning", "Transit.walk", Nothing)
            value' = constructLog ("Use map once", "warning", "Transit.walk", Just "VXNlIG1hcCBvbmNlOndhcm5pbmc6Og==")
         in fill value `shouldBe` value'

constructLog :: (Text, Text, Text, Maybe Text) -> Value
constructLog (rule, level, declaration, partialFingerprint) =
  Object . singleton "runs" $
    Array . Vector.singleton . Object $
      singleton "results" . Array $
        Vector.singleton result
  where
    logicalLocations =
      Array . Vector.singleton . Object $
        fromList [("fullyQualifiedName", String declaration)]
    locations = Array $ Vector.singleton logicalLocations
    partialFingerprints
      | Nothing <- partialFingerprint = []
      | Just fp <- partialFingerprint =
          [ ( "partialFingerprints",
              Object $ singleton "LogicalCodeIssue/v1" (String fp)
            )
          ]
    result =
      Object . fromList $
        partialFingerprints
          ++ [ ("ruleId", String rule),
               ("level", String level),
               ("locations", locations)
             ]
