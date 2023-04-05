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

module Arguments (validate, translate) where

import Data.List (group, sort)
import Data.Maybe (mapMaybe)

validate :: [String] -> Maybe String
validate args
  | [] <- errors = Nothing
  | otherwise = Just $ unlines errors
  where
    errors = mapMaybe forString args ++ map (\s -> "duplicate argument: \"" <> s <> "\"") duplicates
    forString s =
      if '=' `elem` s
        then Nothing
        else Just ("no '=' in \"" <> s <> "\"")
    keys = map (fst . toTuple) args
    duplicates = concatMap (take 1) $ filter ((<) 1 .length) $ group $ sort keys

translate :: [String] -> (FilePath, [String])
translate args = (executable', path' : "-j" : "--sarif" : "--no-exit-code" : flags)
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
    flags = concatMap toFlag $ filter ((==) "binary" . fst) argsMap

toTuple :: String -> (String, String)
toTuple s = (key, drop 1 prefixedValue)
  where
    (key, prefixedValue) = break (== '=') s

toFlag :: (String, String) -> [String]
toFlag _ = []
