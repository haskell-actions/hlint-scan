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
-- Description: Uploads SARIF files to GitHub.
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
--
-- This module is responsible for deriving the call and the settings
-- for uploading a SARIF file to GitHub.  It is also responsible for
-- actually uploading the SARIF file.
--
-- This is only responsible for pure computations.
-- The caller needs to make the actual calls to GitHub.
module Upload (toCall, toSettings, toOutputs) where

import Codec.Compression.GZip
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Base64
import Data.String (fromString)
import Data.Text (Text, unpack)
import GitHub.REST

-- | Converts SARIF output into a GitHub REST API call.
-- It will derive necessary parameters from the system environment.
--
-- See ["Upload an analysis as SARIF data"](https://docs.github.com/en/rest/code-scanning?apiVersion=2022-11-28#upload-an-analysis-as-sarif-data) for more details.
toCall ::
  -- | Environment variables.
  [(String, String)] ->
  -- | SARIF output.
  ByteString ->
  -- | GitHub REST API call.
  Maybe GHEndpoint
toCall env sarifLog
  | Just repo <- repo',
    Just commitSha <- commitSha',
    Just ref <- ref',
    Just workspace <- workspace' =
      Just
        GHEndpoint
          { method = POST,
            endpoint = "/repos/:repo/code-scanning/sarifs",
            endpointVals = ["repo" := repo],
            ghData =
              [ "commit_sha" := commitSha,
                "ref" := ref,
                "sarif" := encodedSarif,
                "checkout_uri" := "file://" <> workspace,
                "tool_name" := ("HLint" :: Text),
                "validate" := True
              ]
          }
  | otherwise = Nothing
  where
    -- Both repository owner and name.
    -- I.e., "<owner>/<name>"
    repo' = lookup "GITHUB_REPOSITORY" env
    commitSha' = lookup "GITHUB_SHA" env
    ref' = lookup "GITHUB_REF" env
    workspace' = lookup "GITHUB_WORKSPACE" env
    encodedSarif = encodeBase64 $ compress sarifLog

-- | Settings for calling the GitHub REST API.
toSettings :: Maybe String -> GitHubSettings
toSettings tok =
  GitHubSettings
    { token = AccessToken . fromString <$> tok,
      userAgent = "github.com/haskell-actions/hlint-scan",
      apiVersion = "2022-11-28"
    }

-- | Converts the response from a GitHub REST API call to GitHub action outputs.
toOutputs :: Value -> [String]
toOutputs (Object response) = concatMap toOutput $ KeyMap.toList response
  where
    toOutput ("id", String sarifId) = ["sarif-id=" <> unpack sarifId]
    toOutput _ = []
toOutputs _ = []
