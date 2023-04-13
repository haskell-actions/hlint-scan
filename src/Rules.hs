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
-- Description: Add rules for GitHub.
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
--
-- At the time of writing, HLint does not add a separate @rules@ object
-- because there is not much that it would add that is not already in the results.
-- However, GitHub would use it for the title of a code scanning issue if available,
-- which is frequently better than using the entire message when it is not available.
--
-- This module adds a @rules@ object to SARIF output if it does not already exist.
-- It will use the rule ID as the name of the rule,
-- which GitHub will use as the title of an issue.
--
-- It would have been nice to include the notes in full descriptions.
-- However, we should not because there can be multiple hints
-- with the same name but different notes.
module Rules (add) where

import Data.Aeson
import Data.Aeson.KeyMap hiding (map)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vector
import Prelude hiding (lookup)

-- | If a @sarifLog@ object does not have a @rules@ object
-- in a @driver@ object inside a @tool@ object, add one based on the results.
-- This will make the code scanning issue titles in GitHub more tidy.
add :: Value -> Value
add (Object v) = Object $ mapWithKey addRulesToRuns v
  where
    addRulesToRuns "runs" (Array us) = Array $ fmap addRulesToRun us
    addRulesToRuns _ u = u

    addRulesToRun (Object u)
      | member "tool" u = Object $ mapWithKey addRulesToTool u
      | otherwise =
          Object $
            insert
              "tool"
              ( Object . singleton "driver" $
                  Object . singleton "rules" $
                    rulesArray
              )
              u
    addRulesToRun u = u

    addRulesToTool "tool" (Object u)
      | member "driver" u = Object $ mapWithKey addRulesToDriver u
      | otherwise = Object $ insert "driver" (Object $ singleton "rules" rulesArray) u
    addRulesToTool _ u = u

    addRulesToDriver "driver" o@(Object u)
      | member "rules" u = o
      | otherwise = Object $ insert "rules" rulesArray u
    addRulesToDriver _ u = u

    rules = Set.unions $ mapWithKey getRulesFromRuns v
    rulesArray = Array $ Vector.fromList $ map formatRule $ Set.toList rules

    getRulesFromRuns "runs" (Array us) = Set.unions $ fmap getRulesFromRun us
    getRulesFromRuns _ _ = Set.empty

    getRulesFromRun (Object u) = Set.unions $ mapWithKey getRulesFromResults u
    getRulesFromRun _ = Set.empty

    getRulesFromResults "results" (Array us) = Set.unions $ fmap getRulesFromResult us
    getRulesFromResults _ _ = Set.empty

    getRulesFromResult (Object u)
      | Just (String r) <- lookup "ruleId" u = Set.singleton r
      | otherwise = Set.empty
    getRulesFromResult _ = Set.empty
add v = v

-- | Formats a object which will be placed in a @rules@ object.
-- Basically the minimum that GitHub requires,
-- although we cannot do much more than use the hint name everywhere.
formatRule :: Text -> Value
formatRule ruleId =
  Object . fromList $
    [ ("id", String ruleId),
      ("name", String ruleId),
      ("shortDescription", Object $ singleton "text" $ String ruleId),
      ("fullDescription", Object $ singleton "text" $ String ruleId)
    ]
