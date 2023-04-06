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

module Upload (toCall) where

import Data.ByteString.Lazy (ByteString)
import Data.Text
import GitHub.REST

toCall :: [(String, String)] -> ByteString -> Maybe GHEndpoint
toCall env _
  | Just owner <- owner',
    Just repo <- repo',
    Just commitSha <- commitSha', Just ref <- ref' =
      Just
        GHEndpoint
          { method = POST,
            endpoint = "/repos/:owner/:repo/code-scanning/sarifs",
            endpointVals = ["owner" := owner, "repo" := repo],
            ghData =
              [ "commit_sha" := commitSha,
                "ref" := ref,
                "sarif" := todo,
                "checkout_uri" := todo,
                "started_at" := todo,
                "tool_name" := ("HLint" :: Text),
                "validate" := True
              ]
          }
  | otherwise = Nothing
  where
    -- Both repository owner and name.
    -- I.e., "<owner>/<name>"
    owner' = lookup "GITHUB_REPOSITORY_OWNER" env
    repo' = lookup "GITHUB_REPOSITORY_NAME" env
    commitSha' = lookup "GITHUB_SHA" env
    ref' = lookup "GITHUB_REF" env

todo :: Text
todo = undefined
