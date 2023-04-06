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

module Fingerprint (fill) where

import Data.Aeson
import Data.Aeson.KeyMap
import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.ByteString.Lazy.Base64 (encodeBase64)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import GHC.Generics (Generic)
import Prelude hiding (lookup)

fill :: Value -> Value
fill (Object v) = Object $ mapWithKey fillRuns v
fill v = v

fillRuns :: Key -> Value -> Value
fillRuns "runs" (Array v) = Array $ fmap fillRun v
fillRuns _ v = v

fillRun :: Value -> Value
fillRun (Object v) = Object $ mapWithKey fillResults v
fillRun v = v

fillResults :: Key -> Value -> Value
fillResults "results" (Array v) = Array $ fmap fillResult v
fillResults _ v = v

fillResult :: Value -> Value
fillResult o@(Object v)
  | member "partialFingerprint" v = o
  | otherwise = Object $ insert "partialFingerprint" fpValue v
  where
    fp = toPartialFingerprint $ toCodeIssue v
    fpValue = Object $ singleton "LogicalCodeIssue/v1" $ String fp
fillResult v = v

data CodeIssue = CodeIssue
  { ruleId :: Maybe Text,
    level :: Maybe Text,
    locations :: Maybe [Text] -- Only the logical locations, i.e., full declaration.
  }
  deriving (Generic)

instance Binary CodeIssue

toCodeIssue :: Object -> CodeIssue
toCodeIssue =
  foldrWithKey
    addToCodeIssue
    CodeIssue
      { ruleId = Nothing,
        level = Nothing,
        locations = Nothing
      }

addToCodeIssue :: Key -> Value -> CodeIssue -> CodeIssue
addToCodeIssue "ruleId" (String s) issue = issue {ruleId = Just s}
addToCodeIssue "level" (String s) issue = issue {level = Just s}
addToCodeIssue "location" (Object v) issue =
  issue {locations = Just $ logicalLocationsFrom v}
addToCodeIssue _ _ issue = issue

logicalLocationsFrom :: Object -> [Text]
logicalLocationsFrom = undefined

toPartialFingerprint :: CodeIssue -> Text
toPartialFingerprint = toStrict . encodeBase64 . Binary.encode
