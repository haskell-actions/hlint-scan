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
-- Description: Adds automation details
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
--
-- Adds run automation details to a @sarifLog@ object.
-- The details include the category and the run ID.
--
-- See [@runAutomationDetails object@](https://docs.github.com/en/code-security/code-scanning/integrating-with-code-scanning/sarif-support-for-code-scanning#runautomationdetails-object) for details.
module AutomationDetails (add) where

import Data.Aeson
import Data.Aeson.KeyMap hiding (lookup, map)
import Data.Maybe (fromMaybe)
import Data.Text hiding (singleton)

-- | Adds run automation details for a @sarifLog@ object.
-- The details in include the category and run ID.
-- If the @sarifLog@ object already has run automation details,
-- it will be overwritten.
add ::
  -- | Environment variables for deriving the run ID.
  [(String, String)] ->
  -- | The category to be associated with the run.
  Maybe String ->
  -- | The @sarifLog@ object to add details to.
  Value ->
  -- | The @sarifLog@ object with details added.
  Value
add env category (Object v) = Object $ mapWithKey addToRuns v
  where
    details = maybe "" (++ "/") category <> "hlint/" <> runId
    runId = fromMaybe "" (lookup "GITHUB_RUN_ID" env)

    addToRuns "runs" (Array u) = Array $ fmap addToRun $ u
    addToRuns _ u = u

    addToRun (Object u) = Object $ addDetails u
    addToRun u = u

    addDetails =
      insert
        "automationDetails"
        (Object $ singleton "id" (String $ pack details))
add _ _ v = v
