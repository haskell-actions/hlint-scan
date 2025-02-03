{-
Copyright 2025 Google LLC

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
-- Description: Output GitHub annotations when code scanning uploads are disabled.
-- Copyright: Copyright 2025 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
--
-- Responsible for the special case where code scanning result uploads are disabled.
-- Instead, the code scanning results are to be output as GitHub annotations instead,
-- and a non-zero exit should be done depending on the highest level present in the results.
--
-- This is only intended for use in private GitHub repositories
-- which do not have code scanning available.
module SpecialOutput (FailOn (..), output) where

import Data.Aeson hiding (Error)
import Data.Aeson.KeyMap hiding (map, mapMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, pack, replace, stripPrefix)
import Data.Vector qualified as Vector
import System.Exit (ExitCode (..))
import Prelude hiding (lookup)

-- | Result level for when to fail with special case where code scanning is disabled.
data FailOn
  = -- | Never fail.
    Never
  | -- | Fail only when there is an error.
    Error
  | -- | Fail when there is a warning or higher.
    Warning
  | -- | Fail when there is a note or higher.
    Note
  deriving (Eq, Show)

-- | Return the GitHub annotations and exit code for the expected failure behavior and SARIF output.
output ::
  -- | Failure behavior.
  FailOn ->
  -- | SARIF output from HLint.
  ByteString ->
  -- | Output and exit code.
  (Text, ExitCode)
output failOn out
  | [] <- rs = ("", ExitSuccess)
  | otherwise = (annotations, exitCode)
  where
    rs = maybe [] fromValue value
    value = decode out :: Maybe Value

    fromValue (Object v) = results v
    fromValue _ = []

    annotations = mconcat $ mapMaybe toAnnotation rs
    exitCode = exitCodeFromResults failOn rs

-- | Returns the result objects from a SARIF object as a list.
results :: Object -> [Object]
results v = maybe [] fromRuns $ lookup "runs" v
  where
    fromRuns (Array us) = concatMap fromRun $ Vector.toList us
    fromRuns _ = []

    fromRun (Object u) = maybe [] fromResults $ lookup "results" u
    fromRun _ = []

    fromResults (Array us) = mapMaybe fromResult $ Vector.toList us
    fromResults _ = []

    fromResult (Object u) = Just u
    fromResult _ = Nothing

-- | Returns which exit code to use for the given failure behavior and result objects.
exitCodeFromResults :: FailOn -> [Object] -> ExitCode
exitCodeFromResults Never _ = ExitSuccess
exitCodeFromResults Error vs = toExitCode ["error"] vs
exitCodeFromResults Warning vs = toExitCode ["error", "warning"] vs
exitCodeFromResults Note vs = toExitCode ["error", "warning", "note"] vs

toExitCode :: [Text] -> [Object] -> ExitCode
toExitCode levels vs
  | hasLevel levels vs = ExitFailure 1
  | otherwise = ExitSuccess

hasLevel :: [Text] -> [Object] -> Bool
hasLevel levels = any has
  where
    has v = maybe False (`elem` levels) (toLevel $ lookup "level" v)
    toLevel (Just (String s)) = Just s
    toLevel _ = Nothing

-- | Converts a result object into a GitHub workflow annotation.
toAnnotation :: Object -> Maybe Text
toAnnotation v = do
  level' <- level
  message' <- message
  title' <- title
  let annotation =
        mconcat
          [ "::" <> level' <> " ",
            location,
            "title=" <> title' <> "::",
            message'
          ]
  return $ escapeNewlines annotation <> "\n"
  where
    level = toGitHubLevel $ lookup "level" v
    message = messageText $ lookup "message" v
    title = ruleId $ lookup "ruleId" v
    location = locationAnnotation v

    -- From SARIF levels to GitHub levels.
    toGitHubLevel (Just (String "error")) = Just "error"
    toGitHubLevel (Just (String "warning")) = Just "warning"
    toGitHubLevel (Just (String "note")) = Just "notice"
    toGitHubLevel _ = Nothing

    messageText (Just (Object u))
      | Just (String s) <- lookup "text" u = Just $ escapeNewlines s
      | otherwise = Nothing
    messageText _ = Nothing

    ruleId (Just (String s)) = Just s
    ruleId _ = Nothing

-- | Returns the annotations for the location in a result object.
-- If there are any location annotations, the return value will end with @", "@,
-- otherwise the return value will be empty.
locationAnnotation :: Object -> Text
locationAnnotation v =
  mconcat
    [ fileAnnotation,
      colAnnotation,
      endColumnAnnotation,
      lineAnnotation,
      endLineAnnotation
    ]
  where
    fileAnnotation
      | (Just s) <- filename = "file=" <> s <> ", "
      | otherwise = ""

    colAnnotation
      | (Just n) <- col = "col=" <> pack (show n) <> ", "
      | otherwise = ""

    endColumnAnnotation
      | (Just n) <- endColumn = "endColumn=" <> pack (show n) <> ", "
      | otherwise = ""

    lineAnnotation
      | (Just n) <- line = "line=" <> pack (show n) <> ", "
      | otherwise = ""

    endLineAnnotation
      | (Just n) <- endLine = "endLine=" <> pack (show n) <> ", "
      | otherwise = ""

    locations
      | Just (Array us) <- lookup "locations" v = Just us
      | otherwise = Nothing

    physicalLocation
      | Just (Object u : _) <- Vector.toList <$> locations,
        Just (Object l) <- lookup "physicalLocation" u =
          Just l
      | otherwise = Nothing

    filename
      | Just (Object u) <- lookup "artifactLocation" =<< physicalLocation,
        Just (String uri) <- lookup "uri" u =
          Just $ fromMaybe uri $ stripPrefix "./" uri
      | otherwise = Nothing

    region
      | Just (Object u) <- lookup "region" =<< physicalLocation = Just u
      | otherwise = Nothing

    col
      | Just (Number n) <- lookup "startColumn" =<< region = Just n
      | otherwise = Nothing

    endColumn
      | Just (Number n) <- lookup "endColumn" =<< region = Just n
      | otherwise = Nothing

    line
      | Just (Number n) <- lookup "startLine" =<< region = Just n
      | otherwise = Nothing

    endLine
      | Just (Number n) <- lookup "endLine" =<< region = Just n
      | otherwise = Nothing

-- | Replace newlines in output so that they can be treated as newlines
-- despite being on a single line annotation.
--
-- See https://github.com/actions/toolkit/issues/193.
escapeNewlines :: Text -> Text
escapeNewlines = replace "\n" "%0A"
