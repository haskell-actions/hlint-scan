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
-- Description: Format messages for GitHub.
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
--
-- Format message text in result objects for GitHub.
--
-- GitHub only supports the `text` property in a `message` object inside a `result` object.
-- It does not support other properties such as `markdown`.
-- Theoretically this means that GitHub should only be able to render messages
-- as pure text, which is unfortunate because it doesn't actually do so
-- and ignores spaces at the beginning of lines, which makes messages
-- much less readable.
--
-- On the flip side, the reason it ignores spaces in the beginning is
-- GitHub renders the messages with Markdown, although unfortunately it
-- renders a restricted and /undocumented/ subset of the full Markdown syntax.
--
-- SARIF output from HLint should support tools in general,
-- so it would not be appropriate for HLint to add Markdown formatting
-- to the text messages.  Instead, this module will rewrite the text messages
-- to add formatting to make them easier to read, since this program
-- is specialized for uploading SARIF files to GitHub.
module Format (formatMessages) where

import Data.Aeson
import Data.Aeson.KeyMap hiding (map)
import Data.Text qualified as Text

-- | Format text messages in result objects to be better readable on GitHub.
formatMessages :: Value -> Value
formatMessages (Object v) = Object (mapWithKey formatRuns v)
  where
    formatRuns "runs" (Array us) = Array $ fmap formatRun us
    formatRuns _ u = u

    formatRun (Object u) = Object $ mapWithKey formatResults u
    formatRun u = u

    formatResults "results" (Array us) = Array $ fmap formatResult us
    formatResults _ u = u

    formatResult (Object u) = Object $ mapWithKey formatMessage u
    formatResult u = u

    formatMessage "message" (Object u) = Object $ mapWithKey formatText u
    formatMessage _ u = u
formatMessages v = v

-- | Formats the text in a @message@ object better for GitHub.
--
-- Basically rewrites the text so that GitHub does not turn the message
-- into mindlessly left-aligned lines of text.  The specific rewriting
-- was derived by trial and error; there appears to be no documentation
-- as to what parts of Markdown syntax are effective in this context,
-- unfortunately.
formatText :: Key -> Value -> Value
formatText "text" (String s) = String s'
  where
    s' = Text.unlines $ map escapeCharacters $ format $ Text.lines s

    -- Put an extra newline between separate pieces of content.
    -- I.e., between the general message, the code found,
    -- the suggested replacements, and any notes.
    -- The extra line is inserted as "  " instead of ""
    -- because GitHub would otherwise collapse them.
    format (x : xs@(x' : _))
      | not (Text.isPrefixOf " " x') = x : "  " : format xs
      | otherwise = x : format xs
    format (x : xs) = x : format xs
    format [] = []

    -- Replace all spaces with @&nbsp;@ so that GitHub does not collapse them.
    -- Also replace backslashes with @&bsol;@ so that
    -- they don't form an accidental escape sequence.
    escapeCharacters =
      Text.replace " " "&nbsp;"
        . Text.replace "\\" "\\\\"
        . Text.replace "&" "&amp;"
formatText _ v = v
