{-
Copyright 2025 Google LLC

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
-- Description: Tests for the "SpecialOutput" module.
-- Copyright: Copyright 2025 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
module SpecialOutputSpec where

import Data.Aeson hiding (Error)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Vector qualified as Vector
import SpecialOutput
import System.Exit
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "outputs minimal annotation" $
    let v = encode $ Object $ KeyMap.singleton "runs" runs
        runs = Array $ Vector.singleton run
        run = Object $ KeyMap.singleton "results" results
        results = Array $ Vector.singleton result
        result =
          Object $
            KeyMap.fromList
              [ ("level", "error"),
                ("ruleId", "redundant entity"),
                ("message", Object $ KeyMap.singleton "text" "random comment")
              ]
     in output Never v `shouldBe` ("::error title=redundant entity::random comment\n", ExitSuccess)

  it "outputs annotation with some location information" $
    let v = encode $ Object $ KeyMap.singleton "runs" runs
        runs = Array $ Vector.singleton run
        run = Object $ KeyMap.singleton "results" results
        results = Array $ Vector.singleton result
        result =
          Object $
            KeyMap.fromList
              [ ("level", "error"),
                ("ruleId", "redundant entity"),
                ("message", Object $ KeyMap.singleton "text" "random comment"),
                ("locations", Array $ Vector.singleton location)
              ]
        location = Object $ KeyMap.singleton "physicalLocation" physicalLocation
        physicalLocation = Object $ KeyMap.singleton "artifactLocation" artifactLocation
        artifactLocation = Object $ KeyMap.singleton "uri" "SpecialOutput.hs"
     in output Never v
          `shouldBe` ("::error file=SpecialOutput.hs,title=redundant entity::random comment\n", ExitSuccess)

  it "outputs annotation with full location information" $
    let v = encode $ Object $ KeyMap.singleton "runs" runs
        runs = Array $ Vector.singleton run
        run = Object $ KeyMap.singleton "results" results
        results = Array $ Vector.singleton result
        result =
          Object $
            KeyMap.fromList
              [ ("level", "error"),
                ("ruleId", "redundant entity"),
                ("message", Object $ KeyMap.singleton "text" "random comment"),
                ("locations", Array $ Vector.singleton location)
              ]
        location = Object $ KeyMap.singleton "physicalLocation" physicalLocation
        physicalLocation =
          Object $
            KeyMap.fromList
              [ ("artifactLocation", artifactLocation),
                ( "region",
                  Object $
                    KeyMap.fromList
                      [ ("startColumn", Number 12),
                        ("endColumn", Number 20),
                        ("startLine", Number 1020),
                        ("endLine", Number 1025)
                      ]
                )
              ]
        artifactLocation = Object $ KeyMap.singleton "uri" "./SpecialOutput.hs"
     in output Never v
          `shouldBe` ( mconcat
                         [ "::error ",
                           "file=SpecialOutput.hs,",
                           "col=12,",
                           "endColumn=20,",
                           "line=1020,",
                           "endLine=1025,",
                           "title=redundant entity::",
                           "random comment\n"
                         ],
                       ExitSuccess
                     )

  it "escapes newlines in messages" $
    let v = encode $ Object $ KeyMap.singleton "runs" runs
        runs = Array $ Vector.singleton run
        run = Object $ KeyMap.singleton "results" results
        results = Array $ Vector.singleton result
        result =
          Object $
            KeyMap.fromList
              [ ("level", "error"),
                ("ruleId", "redundant entity"),
                ("message", Object $ KeyMap.singleton "text" "random\ncomment:2=2")
              ]
     in output Never v
          `shouldBe` ("::error title=redundant entity::random%0Acomment:2=2\n", ExitSuccess)

  it "escapes special characters" $
    let v = encode $ Object $ KeyMap.singleton "runs" runs
        runs = Array $ Vector.singleton run
        run = Object $ KeyMap.singleton "results" results
        results = Array $ Vector.singleton result
        result =
          Object $
            KeyMap.fromList
              [ ("level", "error"),
                ("ruleId", "redundant entity\n:="),
                ("message", Object $ KeyMap.singleton "text" "random comment"),
                ("locations", Array $ Vector.singleton location)
              ]
        location = Object $ KeyMap.singleton "physicalLocation" physicalLocation
        physicalLocation = Object $ KeyMap.singleton "artifactLocation" artifactLocation
        artifactLocation = Object $ KeyMap.singleton "uri" "./SpecialOutput.hs\n:="
     in output Never v
          `shouldBe` ( mconcat
                         [ "::error ",
                           "file=SpecialOutput.hs%0A%3A%3D,",
                           "title=redundant entity%0A%3A%3D::",
                           "random comment\n"
                         ],
                       ExitSuccess
                     )

  let sarif levels = encode $ Object $ KeyMap.singleton "runs" runs
        where
          runs = Array $ Vector.singleton run
          run = Object $ KeyMap.singleton "results" $ results levels
      results levels = Array $ Vector.fromList $ map result levels
      result level =
        Object $
          KeyMap.fromList
            [ ("level", level),
              ("ruleId", "a"),
              ("message", Object $ KeyMap.singleton "text" "b")
            ]
   in describe "exit code" $ do
        it "never : [error]" $
          snd (output Never $ sarif ["error"]) `shouldBe` ExitSuccess

        it "error : [note, warning]" $
          snd (output Error $ sarif ["note", "warning"]) `shouldBe` ExitSuccess

        it "error : [note, warning, error]" $
          snd (output Error $ sarif ["note", "warning", "error"]) `shouldBe` ExitFailure 1

        it "warning : [note, note]" $
          snd (output Warning $ sarif ["note", "note"]) `shouldBe` ExitSuccess

        it "warning : [note, warning]" $
          snd (output Warning $ sarif ["note", "warning"]) `shouldBe` ExitFailure 1

        it "note : []" $
          snd (output Note $ sarif []) `shouldBe` ExitSuccess

        it "note : [note]" $
          snd (output Note $ sarif ["note"]) `shouldBe` ExitFailure 1

        it "note : [warning]" $
          snd (output Note $ sarif ["warning"]) `shouldBe` ExitFailure 1
