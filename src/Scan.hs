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

import Arguments (translate)
import Fingerprint (fill)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.Process (proc, readCreateProcessWithExitCode)
import Upload (toRequest)

main :: IO ()
main = do
  args <- getArgs
  let (executable, flags) = translate args
  (exitCode, out, err) <- readCreateProcessWithExitCode (proc executable flags) ""
  case exitCode of
    ExitSuccess -> send out
    _ -> putStr err >> exitWith exitCode

send :: String -> IO ()
send output = do
  let _ = toRequest output'
  return ()
  where
    output' = fill output
