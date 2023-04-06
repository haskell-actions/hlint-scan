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

module Scan (main) where

import Arguments qualified
import Data.Aeson (Value, decode, encode)
import Data.ByteString.Lazy
import Data.String
import Fingerprint qualified
import GitHub.REST
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess), die, exitWith)
import System.Process (proc, readCreateProcessWithExitCode)
import Upload (toCall, toSettings)
import Prelude hiding (putStr)

main :: [String] -> IO ()
main args = case Arguments.validate args of
  Nothing -> invoke args
  Just errors -> die errors

invoke :: [String] -> IO ()
invoke args = do
  let (executable, flags) = Arguments.translate args
  (exitCode, out, err) <- readCreateProcessWithExitCode (proc executable flags) ""
  case exitCode of
    ExitSuccess -> fingerprint $ fromString out
    _ -> putStrLn err >> exitWith exitCode

fingerprint :: ByteString -> IO ()
fingerprint output = case output' of
  Nothing -> die $ "invalid encoding\n" <> show output <> "\n"
  Just out -> send out
  where
    value = decode output :: Maybe Value
    output' = encode . Fingerprint.fill <$> value

send :: ByteString -> IO ()
send output = do
  env <- getEnvironment
  let settings = toSettings env
  print $ token settings -- remove later
  print env -- remove later
  let endpoint' = toCall env output
  case endpoint' of
    Just endpoint -> call settings endpoint
    _ -> die ("missing environment variables\n" <> show env)

call :: GitHubSettings -> GHEndpoint -> IO ()
call settings endpoint = do
  sarifId <- runGitHubT settings $ do
    ref <- queryGitHub endpoint
    return (ref .: "sarif-id" :: String)
  putStrLn $ "sarif-id=" <> sarifId
