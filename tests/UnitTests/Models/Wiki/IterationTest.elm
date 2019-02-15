module UnitTests.Models.Wiki.IterationTest exposing (..)

import Athenia.Models.Wiki.Iteration as Iteration
import Expect
import Json.Decode as JsonDecode
import Test exposing (..)


testModelDecoder : Test
testModelDecoder =
    describe "Tests multiple decode possibilities to make sure it works"
        [ test "Test minimal decode" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , content = "Some Content"
                                , created_by = Nothing
                                })
                    <| JsonDecode.decodeString Iteration.modelDecoder "{\"id\":342,\"content\":\"Some Content\"}"
        , test "Test decode with created by set" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , content = "Some Content"
                                , created_by = Just { id = 53
                                                   , name = "Barry Manilow"
                                                   , email = "butts@butts.com"
                                                   , password  = ""
                                                   , roles = []
                                                   }
                                })
                    <| JsonDecode.decodeString Iteration.modelDecoder "{\"id\":342,\"content\":\"Some Content\",\"created_by\":{\"id\":53,\"name\":\"Barry Manilow\",\"email\":\"butts@butts.com\"}}"
        ]

testGetContentActionType: Test
testGetContentActionType =
    describe "Tests to make sure that the right content action can be determined"
        [ test "make sure that a matching string returns properly" <|
            \() ->
                Expect.equal Iteration.NoAction
                    <| Iteration.getContentActionType "A piece of content." "A piece of content."
        , test "make sure that an add action returns properly" <|
            \() ->
                Expect.equal (Iteration.Add {start_position = 2, content = "new "})
                    <| Iteration.getContentActionType "A piece of content." "A new piece of content."
        , test "make sure that a remove action returns properly" <|
            \() ->
                Expect.equal (Iteration.Remove {start_position = 1, length = 5})
                    <| Iteration.getContentActionType "An old piece of content." "A piece of content."
        , test "make sure that a replace action returns properly" <|
            \() ->
                Expect.equal (Iteration.Replace {start_position = 1, length = 7, content = " new" })
                    <| Iteration.getContentActionType "An older piece of content." "A new piece of content."
        , test "make sure that a replace action returns properly when the content is shorter" <|
            \() ->
                Expect.equal (Iteration.Replace {start_position = 2, length = 3, content = "recent" })
                    <| Iteration.getContentActionType "A new piece of content." "A recent piece of content."
        ]
