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
-- Description: Format messages for GitHub
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
formatMessages (Object v) = Object $ mapWithKey formatRuns v
formatMessages v = v

formatRuns :: Key -> Value -> Value
formatRuns "runs" (Array vs) = Array $ fmap formatRun vs
formatRuns _ v = v

formatRun :: Value -> Value
formatRun (Object v) = Object $ mapWithKey formatResults v
formatRun v = v

formatResults :: Key -> Value -> Value
formatResults "results" (Array vs) = Array $ fmap formatResult vs
formatResults _ v = v

formatResult :: Value -> Value
formatResult (Object v) = Object $ mapWithKey formatMessage v
formatResult v = v

formatMessage :: Key -> Value -> Value
formatMessage "message" (Object v) = Object $ mapWithKey formatText v
formatMessage _ v = v

formatText :: Key -> Value -> Value
formatText "text" (String s) = String s'
  where
    l = Text.lines s
    s' | x:xs <- l = Text.unlines $ x : "<pre>" : xs ++ ["</pre>"]
       | [] <- l = s
formatText _ v = v
