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

import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Aeson.KeyMap hiding (lookup, map)
import Data.Text hiding (singleton)

-- See https://github.com/github/codeql-action/blob/v2/lib/upload-lib.js
-- See https://docs.github.com/en/code-security/code-scanning/integrating-with-code-scanning/sarif-support-for-code-scanning#runautomationdetails-object

add :: [(String, String)] -> Maybe String -> Value -> Value
add env category (Object v) = Object $ mapWithKey (addToRuns details) v
  where
    details = fromMaybe "" category <> "/" <> runId
    runId = fromMaybe "" (lookup "GITHUB_RUN_ID" env)
add _ _ v = v

addToRuns :: String -> Key -> Value -> Value
addToRuns details "runs" (Array v) =
  Array $ fmap (addToRun details) v
addToRuns _ _ v = v

addToRun :: String -> Value -> Value
addToRun details (Object v) = Object $ addDetails details v
addToRun _ v = v

addDetails :: String -> Object -> Object
addDetails details =
  insert
    "automationDetails"
    (Object $ singleton "id" (String $ pack details))
