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

module FilePath (normalize) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text

normalize :: Value -> Value
normalize (Object v) = Object $ normalizeObject v
normalize (Array vs) = Array $ fmap normalize vs
normalize v = v

normalizeObject :: Object -> Object
normalizeObject m | Just v <- KeyMap.lookup "artifactLocation" m =
                      KeyMap.insert "artifactLocation" (normalizeUri v) m
                  | otherwise = m

normalizeUri :: Value -> Value
normalizeUri (Object m) | Just (String s) <- KeyMap.lookup "uri" m =
                            Object $ KeyMap.insert "uri" (String $ strip s) m
  where
    strip x | Just x' <- Text.stripPrefix "./" x = strip x'
            | otherwise = x
normalizeUri v = v
