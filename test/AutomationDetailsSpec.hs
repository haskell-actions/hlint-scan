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
-- Description: Tests for the "AutomationDetails" module.
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
module AutomationDetailsSpec (spec) where

import AutomationDetails
import Data.Aeson.KeyMap
import Data.Aeson.Types
import Data.String (fromString)
import Data.Vector qualified as Vector
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "add" $ do
    prop "adds category and run ID" $ \category runId ->
      add [("GITHUB_RUN_ID", fromString runId)] (Just $ fromString category) emptySarifLog
        `shouldBe` Object
          ( singleton "runs" $
              Array . Vector.singleton . Object $
                singleton "automationDetails" $
                  Object $
                    singleton "id" $
                      String (fromString category <> "/hlint/" <> fromString runId)
          )

    prop "adds empty category and empty run ID when not available" $
      add [] Nothing emptySarifLog
        `shouldBe` Object
          ( singleton "runs" $
              Array . Vector.singleton . Object $
                singleton "automationDetails" $
                  Object $
                    singleton "id" $
                      String "hlint/"
          )
  where
    emptySarifLog = Object $ singleton "runs" $ Array $ Vector.singleton emptyObject
