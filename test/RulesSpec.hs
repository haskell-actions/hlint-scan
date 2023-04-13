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
-- Description: Tests for the "Rules" module.
-- Copyright: Copyright 2023 Google LLC
-- License: Apache-2.0
-- Maintainer: chungyc@google.com
module RulesSpec (spec) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Rules
import Test.Hspec

spec :: Spec
spec = do
  describe "add" $ do
    it "adds rules when there was no tool object" $
      add
        ( Object . KeyMap.singleton "runs" $
            Array . Vector.fromList $
              [ Object . KeyMap.singleton "results" . Array . Vector.fromList $
                  [ Object . KeyMap.fromList $
                      [ ("ruleId", String "use 1")
                      ],
                    Object . KeyMap.fromList $
                      [ ("ruleId", String "use 2"),
                        ( "message",
                          Object . KeyMap.singleton "text" . String $
                            Text.unlines
                              [ "Some warning",
                                "Found:",
                                "  not (x-y < 0 || x-y > 0)",
                                "Perhaps:",
                                "  x == y",
                                "Note: this is just better"
                              ]
                        )
                      ]
                  ]
              ]
        )
        `shouldBe` Object
          ( KeyMap.singleton "runs" $
              Array . Vector.singleton . Object . KeyMap.fromList $
                [ ( "tool",
                    Object . KeyMap.singleton "driver" $
                      Object . KeyMap.singleton "rules" $
                        Array . Vector.fromList $
                          [ Object . KeyMap.fromList $
                              [ ("id", String "use 1"),
                                ("name", String "use 1"),
                                ( "shortDescription",
                                  Object $ KeyMap.singleton "text" $ String "use 1"
                                ),
                                ( "fullDescription",
                                  Object $ KeyMap.singleton "text" $ String "use 1"
                                )
                              ],
                            Object . KeyMap.fromList $
                              [ ("id", String "use 2"),
                                ("name", String "use 2"),
                                ( "shortDescription",
                                  Object . KeyMap.singleton "text" $ String "use 2"
                                ),
                                ( "fullDescription",
                                  Object . KeyMap.singleton "text" $ String "use 2"
                                )
                              ]
                          ]
                  ),
                  ( "results",
                    Array . Vector.fromList $
                      [ Object . KeyMap.fromList $
                          [ ("ruleId", String "use 1")
                          ],
                        Object . KeyMap.fromList $
                          [ ("ruleId", String "use 2"),
                            ( "message",
                              Object . KeyMap.singleton "text" . String $
                                Text.unlines
                                  [ "Some warning",
                                    "Found:",
                                    "  not (x-y < 0 || x-y > 0)",
                                    "Perhaps:",
                                    "  x == y",
                                    "Note: this is just better"
                                  ]
                            )
                          ]
                      ]
                  )
                ]
          )

    it "adds rules when there was no driver object" $
      add
        ( Object . KeyMap.singleton "runs" $
            Array . Vector.fromList $
              [ Object . KeyMap.fromList $
                  [ ( "tool",
                      Object . KeyMap.singleton "extensions" . Array . Vector.singleton $
                        Object . KeyMap.singleton "name" $
                          String "random extension"
                    ),
                    ( "results",
                      Array . Vector.fromList $
                        [ Object . KeyMap.fromList $
                            [ ("ruleId", String "use 1")
                            ]
                        ]
                    )
                  ]
              ]
        )
        `shouldBe` Object
          ( KeyMap.singleton "runs" $
              Array . Vector.singleton . Object . KeyMap.fromList $
                [ ( "tool",
                    Object . KeyMap.fromList $
                      [ ( "driver",
                          Object . KeyMap.singleton "rules" $
                            Array . Vector.fromList $
                              [ Object . KeyMap.fromList $
                                  [ ("id", String "use 1"),
                                    ("name", String "use 1"),
                                    ( "shortDescription",
                                      Object $ KeyMap.singleton "text" $ String "use 1"
                                    ),
                                    ( "fullDescription",
                                      Object $ KeyMap.singleton "text" $ String "use 1"
                                    )
                                  ]
                              ]
                        ),
                        ( "extensions",
                          Array . Vector.singleton $
                            Object . KeyMap.singleton "name" $
                              String "random extension"
                        )
                      ]
                  ),
                  ( "results",
                    Array . Vector.fromList $
                      [ Object . KeyMap.fromList $
                          [("ruleId", String "use 1")]
                      ]
                  )
                ]
          )

    it "does not overwrite existing rules" $
      add
        ( Object . KeyMap.singleton "runs" $
            Array . Vector.fromList $
              [ Object . KeyMap.fromList $
                  [ ( "tool",
                      Object . KeyMap.singleton "driver" . Object $
                        KeyMap.singleton "rules" . Array . Vector.fromList $
                          [ Object . KeyMap.fromList $
                              [ ("id", String "use 1"),
                                ("name", String "The first rule"),
                                ( "shortDescription",
                                  Object $ KeyMap.singleton "text" $ String "See rule 1"
                                ),
                                ( "fullDescription",
                                  Object $ KeyMap.singleton "text" $ String "Really see rule 1"
                                )
                              ]
                          ]
                    ),
                    ( "results",
                      Array . Vector.fromList $
                        [ Object . KeyMap.fromList $
                            [ ("ruleId", String "use 1")
                            ]
                        ]
                    )
                  ]
              ]
        )
        `shouldBe` Object
          ( KeyMap.singleton "runs" $
              Array . Vector.singleton . Object . KeyMap.fromList $
                [ ( "tool",
                    Object . KeyMap.fromList $
                      [ ( "driver",
                          Object . KeyMap.singleton "rules" $
                            Array . Vector.fromList $
                              [ Object . KeyMap.fromList $
                                  [ ("id", String "use 1"),
                                    ("name", String "The first rule"),
                                    ( "shortDescription",
                                      Object $ KeyMap.singleton "text" $ String "See rule 1"
                                    ),
                                    ( "fullDescription",
                                      Object $ KeyMap.singleton "text" $ String "Really see rule 1"
                                    )
                                  ]
                              ]
                        )
                      ]
                  ),
                  ( "results",
                    Array . Vector.fromList $
                      [ Object . KeyMap.fromList $
                          [("ruleId", String "use 1")]
                      ]
                  )
                ]
          )
