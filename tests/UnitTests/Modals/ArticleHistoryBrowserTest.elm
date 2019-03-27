module UnitTests.Modals.ArticleHistoryBrowserTest exposing (..)

import Expect
import Modals.ArticleHistoryBrowser as ArticleHistoryBrowser
import Test exposing (..)
import Time exposing (..)


testDetermineIfIterationIsSameSession: Test
testDetermineIfIterationIsSameSession =
    describe "Make sure that we can determine the session off iterations properly"
        [ test "Should return true when same user and within an hour" <|
            \() ->
                let
                    iterationA = { id = 23
                                 , content = ""
                                 , created_at = Time.millisToPosix (60 * 60 * 1000)
                                 , created_by_id = 5
                                 , created_by = Nothing
                                 }
                    iterationB = { id = 35
                                 , content = ""
                                 , created_at = Time.millisToPosix (119 * 60 * 1000)
                                 , created_by_id = 5
                                 , created_by = Nothing
                                 }
                in
                Expect.true ""
                    <| ArticleHistoryBrowser.determineIfIterationIsSameSession iterationA iterationB
        , test "Should return false when same users but the created at time is outside an hour" <|
            \() ->
                let
                    iterationA = { id = 23
                                 , content = ""
                                 , created_at = Time.millisToPosix (60 * 60 * 1000)
                                 , created_by_id = 5
                                 , created_by = Nothing
                                 }
                    iterationB = { id = 35
                                 , content = ""
                                 , created_at = Time.millisToPosix (121 * 60 * 1000)
                                 , created_by_id = 5
                                 , created_by = Nothing
                                 }
                in
                Expect.false ""
                    <| ArticleHistoryBrowser.determineIfIterationIsSameSession iterationA iterationB
        , test "Should return false when the users are not the same" <|
            \() ->
                let
                    iterationA = { id = 23
                                 , content = ""
                                 , created_at = Time.millisToPosix (60 * 60 * 1000)
                                 , created_by_id = 5
                                 , created_by = Nothing
                                 }
                    iterationB = { id = 35
                                 , content = ""
                                 , created_at = Time.millisToPosix (60 * 60 * 1000)
                                 , created_by_id = 6
                                 , created_by = Nothing
                                 }
                in
                Expect.false ""
                    <| ArticleHistoryBrowser.determineIfIterationIsSameSession iterationA iterationB
        ]


testGroupIterations: Test
testGroupIterations =
    describe "Makes sure that we can group iterations correct"
        [ test "Makes sure to filter out all from the same session with none selected yet" <|
            \() ->
                let
                    iterations =
                        [ { id = 23
                          , content = ""
                          , created_at = Time.millisToPosix (169 * 60 * 1000)
                          , created_by_id = 5
                          , created_by = Nothing
                          }
                        , { id = 35
                          , content = ""
                          , created_at = Time.millisToPosix (119 * 60 * 1000)
                          , created_by_id = 5
                          , created_by = Nothing
                          }
                        , { id = 38
                          , content = ""
                          , created_at = Time.millisToPosix (60 * 60 * 1000)
                          , created_by_id = 5
                          , created_by = Nothing
                          }
                        ]
                in
                Expect.equal [{ id = 23
                              , content = ""
                              , created_at = Time.millisToPosix (169 * 60 * 1000)
                              , created_by_id = 5
                              , created_by = Nothing
                              }
                            , { id = 38
                              , content = ""
                              , created_at = Time.millisToPosix (60 * 60 * 1000)
                              , created_by_id = 5
                              , created_by = Nothing
                            }]
                            <| ArticleHistoryBrowser.groupIterations [] iterations
        , test "Makes sure to separate all sessions when they are a different user" <|
            \() ->
                let
                    iterations =
                        [ { id = 23
                          , content = ""
                          , created_at = Time.millisToPosix (169 * 60 * 1000)
                          , created_by_id = 5
                          , created_by = Nothing
                          }
                        , { id = 35
                          , content = ""
                          , created_at = Time.millisToPosix (119 * 60 * 1000)
                          , created_by_id = 6
                          , created_by = Nothing
                          }
                        , { id = 38
                          , content = ""
                          , created_at = Time.millisToPosix (60 * 60 * 1000)
                          , created_by_id = 5
                          , created_by = Nothing
                          }
                        ]
                in
                Expect.equal [ { id = 23
                               , content = ""
                               , created_at = Time.millisToPosix (169 * 60 * 1000)
                               , created_by_id = 5
                               , created_by = Nothing
                               }
                             , { id = 35
                               , content = ""
                               , created_at = Time.millisToPosix (119 * 60 * 1000)
                               , created_by_id = 6
                               , created_by = Nothing
                               }
                             , { id = 38
                               , content = ""
                               , created_at = Time.millisToPosix (60 * 60 * 1000)
                               , created_by_id = 5
                               , created_by = Nothing
                               }
                             ]
                            <| ArticleHistoryBrowser.groupIterations [] iterations
        ]
