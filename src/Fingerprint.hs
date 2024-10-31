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
import Data.Base64.Types (extractBase64)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding.Base64 (encodeBase64)
import Data.Vector qualified as Vector
import Prelude hiding (concatMap, lookup)

-- | For a top-level @sarifLog@ object, fill partial fingerprints
-- in result objects which do not already have them
fill :: Value -> Value
fill (Object v) = Object $ mapWithKey fillRuns v
fill v = v

-- | Fill partial fingerprints for run objects.
fillRuns :: Key -> Value -> Value
fillRuns "runs" (Array v) = Array $ fmap fillRun v
fillRuns _ v = v

-- | Fill partial fingerprints for a run object.
fillRun :: Value -> Value
fillRun (Object v) = Object $ mapWithKey fillResults v
fillRun v = v

-- | Fill partial fingerprints for result objects.
fillResults :: Key -> Value -> Value
fillResults "results" (Array v) = Array $ fmap fillResult v
fillResults _ v = v

-- | Fill partial fingerprint for a result object.
fillResult :: Value -> Value
fillResult o@(Object v)
  | member "partialFingerprint" v = o
  | otherwise = Object $ insert "partialFingerprints" fp v
  where
    fp = toPartialFingerprint v
fillResult v = v

-- | A code issue is encoded in this form and then encoded in Base64.
-- This should ensure that a partial fingerprint does not depend
-- on the order for which any of these elements appear.
data CodeIssue = CodeIssue
  { ruleId :: Maybe Text,
    level :: Maybe Text,
    -- Only the logical locations, i.e., full declarations.
    -- File names are already in other parts of @sarifLog@ object.
    -- Physical locations are sensitive to line and column numbers,
    -- which can frequently be due to changes that
    -- are not relevant for a particular issue.
    locations :: Maybe [Text]
  }

-- | Derives a code issue value from a result object.
toCodeIssue :: Object -> CodeIssue
toCodeIssue =
  foldrWithKey
    addToCodeIssue
    CodeIssue {ruleId = Nothing, level = Nothing, locations = Nothing}

-- | Build up a code issue value from the fields of a result object.
addToCodeIssue :: Key -> Value -> CodeIssue -> CodeIssue
addToCodeIssue "ruleId" (String s) issue = issue {ruleId = Just s}
addToCodeIssue "level" (String s) issue = issue {level = Just s}
addToCodeIssue "location" (Object v) issue =
  issue {locations = Just $ logicalLocationsFrom v}
addToCodeIssue _ _ issue = issue

-- | Retrieves a list of fully qualified names to serve as the logical location.
-- The list will be in sorted order for stability of the partial fingerprint.
logicalLocationsFrom :: Object -> [Text]
logicalLocationsFrom v
  | Just (Array xs) <- logicalLocations =
      sort $ Vector.toList $ Vector.mapMaybe qualifiedName xs
  | otherwise = []
  where
    logicalLocations = lookup "logicalLocations" v

    qualifiedName :: Value -> Maybe Text
    qualifiedName (Object u)
      | Just (String s) <- lookup "fullyQualifiedName" u = Just s
      | otherwise = Nothing
    qualifiedName _ = Nothing

-- | Derive a partial fingerprint from a code issue value.
--
-- It is encoded in a way that should guaranteed to be
-- stable while still distinguishing between different code issue values.
toPartialFingerprint :: Object -> Value
toPartialFingerprint v =
  Object $ singleton propertyName $ String encodedResult
  where
    CodeIssue {ruleId, level, locations} = toCodeIssue v
    encodedLocations = encodeTextList . map Just <$> locations
    encodedResult = encodeTextList [ruleId, level, encodedLocations]
    propertyName = "LogicalCodeIssue/v1"

-- | Encode a list of optional text strings with Base64.
encodeTextList :: [Maybe Text] -> Text
encodeTextList = extractBase64 . encodeBase64 . Text.concat . map encodeItem
  where
    encodeItem Nothing = ":"
    encodeItem (Just s) = s <> ":"
