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
-- Description: Fills partial fingerprints.
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
--
-- SARIF uses partial fingerprints in results to aid in an attempt
-- to track the "same" issues despite changes.  This fills partial
-- fingerprints in result objects which do not already have them,
-- while keeping everything else the same in SARIF output.
module Fingerprint (fill) where

import Data.Aeson
import Data.Aeson.KeyMap hiding (map)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding.Base64 (encodeBase64)
import Data.Vector qualified as Vector
import Prelude hiding (concatMap, lookup)

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
  | otherwise = Object $ insert "partialFingerprints" fp v
  where
    fp = toPartialFingerprint v
fillResult v = v

data CodeIssue = CodeIssue
  { ruleId :: Maybe Text,
    level :: Maybe Text,
    locations :: Maybe [Text] -- Only the logical locations, i.e., full declarations.
  }

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
logicalLocationsFrom v
  | Just (Array xs) <- logicalLocations =
      Vector.toList $ Vector.mapMaybe qualifiedName xs
  | otherwise = []
  where
    logicalLocations = lookup "logicalLocations" v

qualifiedName :: Value -> Maybe Text
qualifiedName (Object v)
  | Just (String s) <- lookup "fullyQualifiedName" v = Just s
  | otherwise = Nothing
qualifiedName _ = Nothing

toPartialFingerprint :: Object -> Value
toPartialFingerprint v =
  Object $ singleton propertyName $ String encodedResult
  where
    CodeIssue {ruleId, level, locations} = toCodeIssue v
    encodedLocations = encodeTextList . map Just <$> locations
    encodedResult = encodeTextList [ruleId, level, encodedLocations]
    propertyName = "LogicalCodeIssue/v1"

encodeTextList :: [Maybe Text] -> Text
encodeTextList = encodeBase64 . Text.concat . map encodeItem
  where
    encodeItem Nothing = ":"
    encodeItem (Just s) = s <> ":"
