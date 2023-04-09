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

module Upload (toCall, toSettings, toOutputs) where

import Codec.Compression.GZip
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Base64
import Data.String (fromString)
import Data.Text (Text, unpack)
import GitHub.REST

toCall :: [(String, String)] -> ByteString -> Maybe GHEndpoint
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

toSettings :: Maybe String -> GitHubSettings
toSettings tok =
  GitHubSettings
    { token = AccessToken . fromString <$> tok,
      userAgent = "https://github.com/haskell-actions/hlint-scan",
      apiVersion = "v3"
    }

toOutputs :: Value -> [String]
toOutputs (Object response) = concatMap toOutput $ KeyMap.toList response
  where
    toOutput ("id", String sarifId) = ["sarif-id=" <> unpack sarifId]
    toOutput ("url", String url) = ["sarif-url=" <> unpack url]
    toOutput _ = []
toOutputs _ = []
