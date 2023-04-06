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

{- |
Description: Hooks up sub-modules together.
Copyright: Copyright 2023 Google LLC
License: Apache-2.0
Maintainer: chungyc@google.com

This is a GitHub action which scans Haskell code
with [HLint](https://github.com/ndmitchell/hlint), which it then uploads
to [GitHub code scanning dashboards](https://docs.github.com/en/code-security/code-scanning/automatically-scanning-your-code-for-vulnerabilities-and-errors/about-code-scanning).
See [haskell-actions/hlint-scan](https://github.com/haskell-actions/hlint-scan)
for more details.

This specific module ties together sub-modules which are responsible
for processing various stages of the analysis and upload.
-}
module Scan (main) where

import Arguments qualified
import AutomationDetails qualified
import Data.Aeson (Value, decode, encode)
import Data.ByteString.Lazy
import Data.String
import Fingerprint qualified
import GitHub.REST
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess), die, exitWith)
import System.Process (proc, readCreateProcessWithExitCode)
import Upload (toCall, toOutputs, toSettings)
import Prelude hiding (lookup, putStr)

-- | Context that will be carried through most of the work flow.
--
-- In particular, this is used to pass on the category and access token
-- which would have been passed in as arguments to the program
-- from the argument parsing stage to the API call to GitHub.
data Context = Context {category :: Maybe String, gitHubToken :: Maybe String}

main :: [String] -> IO ()
main args = case Arguments.validate args of
  Nothing -> invoke args
  Just errors -> die errors

invoke :: [String] -> IO ()
invoke args = do
  let (executable, flags, cat, tok) = Arguments.translate args
  (exitCode, out, err) <- readCreateProcessWithExitCode (proc executable flags) ""
  case exitCode of
    ExitSuccess -> annotate Context {category = cat, gitHubToken = tok} (fromString out)
    _ -> putStrLn err >> exitWith exitCode

annotate :: Context -> ByteString -> IO ()
annotate context output = do
  env <- getEnvironment
  let annotated = AutomationDetails.add env (category context) <$> value
  let annotated' = Fingerprint.fill <$> annotated
  case annotated' of
    Nothing -> die $ "invalid encoding\n" <> show output <> "\n"
    Just output' -> send context (encode output')
  where
    value = decode output :: Maybe Value

send :: Context -> ByteString -> IO ()
send context output = do
  env <- getEnvironment
  let settings = toSettings $ gitHubToken context
  let endpoint' = toCall env output
  case endpoint' of
    Just endpoint -> call settings endpoint
    _ -> die ("missing environment variables\n" <> show env)

call :: GitHubSettings -> GHEndpoint -> IO ()
call settings endpoint = do
  putStrLn . unlines . toOutputs =<< runGitHubT settings (queryGitHub endpoint)
