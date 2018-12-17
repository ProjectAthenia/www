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