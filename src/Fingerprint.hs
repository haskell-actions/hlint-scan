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

module Fingerprint (fill) where

import Data.Aeson
import Data.Aeson.KeyMap

fill :: Value -> Value
fill (Object v) = Object $ mapWithKey fillRuns v
fill v = v

fillRuns :: Key -> Value -> Value
fillRuns "runs" (Array v) = Array $ fmap fillRun v
fillRuns _ v = v

fillRun :: Value -> Value
fillRun (Object v) = Object $ mapWithKey fillResult v
fillRun v = v

fillResult :: Key -> Value -> Value
fillResult = undefined
  -- Fingerprint on level, logical location, content
