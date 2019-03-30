module UnitTests.Models.RoleTest exposing (..)

import Expect
import Json.Decode as JsonDecode
import Json.Encode as JsonEncode
import Models.Role as Role
import Test exposing (..)

testCacheEncoder : Test
testCacheEncoder =
   test "Makes sure that the cache encoder works properly" <|
        \() ->
            let
                model =
                    { id = 42
                    , name = "hi"
                    }
            in
                Expect.equal "{\"id\":42,\"name\":\"\"}"
                    <| JsonEncode.encode 0
                        <| Role.cacheEncoder model

testModelDecoder : Test
testModelDecoder =
    describe "Tests multiple decode possibilities to make sure it works"
        [ test "Test minimal decode" <|
            \() ->
                Expect.equal (Ok { id = 1
                                , name = "A role"
                                })
                    <| JsonDecode.decodeString Role.modelDecoder "{\"id\":1,\"name\":\"A role\"}"
        ]
