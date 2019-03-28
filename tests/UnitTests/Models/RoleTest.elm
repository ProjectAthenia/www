module UnitTests.Models.RoleTest exposing (..)

import Expect
import Json.Decode as JsonDecode
import Models.Role as Role
import Test exposing (..)


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
