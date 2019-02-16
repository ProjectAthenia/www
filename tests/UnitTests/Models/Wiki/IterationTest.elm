module UnitTests.Models.Wiki.IterationTest exposing (..)

import Athenia.Models.Wiki.Iteration as Iteration
import Expect
import Json.Decode as JsonDecode
import Test exposing (..)
import Time exposing (..)


testModelDecoder : Test
testModelDecoder =
    describe "Tests multiple decode possibilities to make sure it works"
        [ test "Test minimal decode" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , content = "Some Content"
                                , created_at = millisToPosix 1000
                                , created_by = Nothing
                                })
                    <| JsonDecode.decodeString Iteration.modelDecoder "{\"id\":342,\"content\":\"Some Content\",\"created_at\":\"1970-01-01T00:00:01+00:00\"}"
        , test "Test decode with created by set" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , content = "Some Content"
                                , created_at = millisToPosix 1000
                                , created_by = Just { id = 53
                                                   , name = "Barry Manilow"
                                                   , email = "butts@butts.com"
                                                   , password  = ""
                                                   , roles = []
                                                   }
                                })
                    <| JsonDecode.decodeString Iteration.modelDecoder "{\"id\":342,\"content\":\"Some Content\",\"created_at\":\"1970-01-01T00:00:01+00:00\",\"created_by\":{\"id\":53,\"name\":\"Barry Manilow\",\"email\":\"butts@butts.com\"}}"
        ]


testEncodeAction: Test
testEncodeAction =
    describe "Tests to make sure the encode action works properly"
        [ test "make sure that no action sends back an empty string" <|
            \() ->
                Expect.equal ""
                    <| Iteration.encodeAction Iteration.NoAction
        , test "make sure that an add action is encoded properly" <|
            \() ->
                Expect.equal "{\"action\":\"add\",\"start_position\":1,\"content\":\"hello\"}"
                    <| Iteration.encodeAction
                        <| Iteration.Add {start_position = 1, content = "hello"}
        , test "make sure that a remove action is encoded properly" <|
            \() ->
                Expect.equal "{\"action\":\"remove\",\"start_position\":1,\"length\":5}"
                    <| Iteration.encodeAction
                        <| Iteration.Remove {start_position = 1, length = 5}
        , test "make sure that a replace action is encoded properly" <|
            \() ->
                Expect.equal "{\"action\":\"replace\",\"start_position\":1,\"length\":5,\"content\":\"hello\"}"
                    <| Iteration.encodeAction
                        <| Iteration.Replace {start_position = 1, length = 5, content = "hello"}
        ]


testGetContentActionType: Test
testGetContentActionType =
    describe "Tests to make sure that the right content action can be determined"
        [ test "make sure that a matching string returns properly" <|
            \() ->
                Expect.equal Iteration.NoAction
                    <| Iteration.getContentActionType "A piece of content." "A piece of content."
        , test "make sure that an add action returns properly when setting initial content" <|
            \() ->
                Expect.equal (Iteration.Add {start_position = 0, content = "Hello"})
                    <| Iteration.getContentActionType "" "Hello"
        , test "make sure that an add action returns properly when adding to the end" <|
            \() ->
                Expect.equal (Iteration.Add {start_position = 5, content = " here's some more"})
                    <| Iteration.getContentActionType "Hello" "Hello here's some more"
        , test "make sure that an add action returns properly" <|
            \() ->
                Expect.equal (Iteration.Add {start_position = 2, content = "new "})
                    <| Iteration.getContentActionType "A piece of content." "A new piece of content."
        , test "make sure that headers con be changed" <|
            \() ->
                Expect.equal (Iteration.Add {start_position = 2, content = "#"})
                    <| Iteration.getContentActionType "## Overview" "### Overview"
        , test "make sure that a remove action returns properly" <|
            \() ->
                Expect.equal (Iteration.Remove {start_position = 1, length = 5})
                    <| Iteration.getContentActionType "An old piece of content." "A piece of content."
        , test "make sure that a remove action returns properly with the last bit of content" <|
            \() ->
                Expect.equal (Iteration.Remove {start_position = 0, length = 3})
                    <| Iteration.getContentActionType "Bye" ""
        , test "make sure that a remove action returns properly for changing a header level" <|
            \() ->
                Expect.equal (Iteration.Remove {start_position = 2, length = 1})
                    <| Iteration.getContentActionType "### Overview" "## Overview"
        , test "make sure that a replace action returns properly" <|
            \() ->
                Expect.equal (Iteration.Replace {start_position = 1, length = 7, content = " new" })
                    <| Iteration.getContentActionType "An older piece of content." "A new piece of content."
        , test "make sure that a replace action returns properly when the content is shorter" <|
            \() ->
                Expect.equal (Iteration.Replace {start_position = 2, length = 3, content = "recent" })
                    <| Iteration.getContentActionType "A new piece of content." "A recent piece of content."
        ]
