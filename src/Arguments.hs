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
-- Description: Translates program arguments to HLint arguments
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
--
-- Translates arguments given to this program into arguments for HLint.
-- It will also derive other parameters such as the category and access token
-- from the arguments.
--
-- Arguments to this program are in the form:
--
-- > <keyword>=<value>
--
-- No keyword will have the @=@ character, so everything after it will be part of the value.
-- This should even include newlines, because each argument in @action.yaml@ will
-- be passed to the program as separate arguments without parsing from the shell.
module Arguments (validate, translate) where

import Data.List (group, sort)
import Data.Maybe (mapMaybe)

-- | Validate the program arguments.
--
-- This checks for two things:
--
-- * That every argument starts with a keyword and a @=@ character.
-- * There are no two arguments which have the same keyword.
--
-- If there are errors found among the arguments, this will return
-- an error message the caller can print, which could be more than one line.
validate ::
  -- | Program arguments.
  [String] ->
  -- | If not 'Nothing', the error message.
  Maybe String
validate args
  | [] <- errors = Nothing
  | otherwise = Just $ unlines errors
  where
    errors = notPairErrors ++ duplicateErrors ++ notAllowedErrors

    -- Find arguments not in the form @keyword=value@.
    notPairErrors = mapMaybe forString args
    forString s
      | '=' `elem` s = Nothing
      | otherwise = Just ("no '=' in \"" <> s <> "\"")

    -- Gather keywords which appear more than once.
    duplicateErrors = map (\s -> "duplicate argument: \"" <> s <> "\"") duplicates
    keys = map (fst . toTuple) args
    duplicates = concatMap (take 1) $ filter ((<) 1 . length) $ group $ sort keys

    -- Look for keywords that are not allowed.
    notAllowedErrors = mapMaybe (notAllowed . fst . toTuple) args
    notAllowed key
      | key `elem` allowedArgs = Nothing
      | otherwise = Just $ "\"" <> key <> "\" argument is not allowed"

-- | List of argument keywords which are allowed.
-- In other words, these are arguments we know what to do with.
allowedArgs :: [String]
allowedArgs = ["binary", "path", "category", "token"]

-- | Translate program arguments to arguments for HLint.
-- Also derives the category and access token from the arguments.
--
-- This does not validate the arguments, which is assumed to have already
-- been done by 'validate'.
translate ::
  -- | The program arguments.
  [String] ->
  -- | In order:
  --
  -- * Executable path for HLint.
  -- * Command-line arguments for HLint.
  -- * Category to upload with.
  -- * GitHub access token.
  (FilePath, [String], Maybe String, Maybe String)
translate args = (executable', path' : requiredFlags, category', token')
  where
    argsMap = map toTuple args

    executable = lookup "binary" argsMap
    executable'
      | Nothing <- executable = "/hlint"
      | Just "" <- executable = "/hlint"
      | Just s <- executable = s

    path = lookup "path" argsMap
    path'
      | Nothing <- path = "."
      | Just "" <- path = "."
      | Just s <- path = s

    category = lookup "category" argsMap
    category'
      | Just "" <- category = Nothing
      | otherwise = category

    token = lookup "token" argsMap
    token'
      | Just "" <- token = Nothing
      | otherwise = token

    requiredFlags = ["-j", "--sarif", "--no-exit-code"]

-- | Converts a program argument of the form @keyword=value@
-- to a tuple of @(keyword, value)@.
toTuple :: String -> (String, String)
toTuple s = (key, drop 1 prefixedValue)
  where
    (key, prefixedValue) = break (== '=') s
