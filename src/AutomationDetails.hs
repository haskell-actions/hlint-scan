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

module AutomationDetails (add) where

import Data.Aeson
import Data.Aeson.KeyMap hiding (lookup, map)
import Data.Maybe (fromMaybe)
import Data.Text hiding (singleton)

-- See https://docs.github.com/en/code-security/code-scanning/integrating-with-code-scanning/sarif-support-for-code-scanning#runautomationdetails-object

add :: [(String, String)] -> Maybe String -> Value -> Value
add env category (Object v) = Object $ mapWithKey addToRuns v
  where
    details = fromMaybe "" category <> "/" <> runId
    runId = fromMaybe "" (lookup "GITHUB_RUN_ID" env)

    addToRuns "runs" (Array u) = Array $ fmap addToRun u
    addToRuns _ u = u

    addToRun (Object u) = Object $ addDetails u
    addToRun u = u

    addDetails =
      insert
        "automationDetails"
        (Object $ singleton "id" (String $ pack details))
add _ _ v = v
