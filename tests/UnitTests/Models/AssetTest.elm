module UnitTests.Models.AssetTest exposing(..)

import Expect
import Json.Decode as JsonDecode
import Models.Asset as Asset
import Test exposing (..)


testModel : Test
testModel =
    describe "Tests to make sure that the model can be created properly"
        [ test "Checks to see that the id was set properly" <|
            \() ->
                Expect.equal 43
                    <| (Asset.model 43 "hello").id
        , test "Checks to see that the url was set properly" <|
            \() ->
                Expect.equal "hello"
                    <| (Asset.model 43 "hello").url
        ]


testModelDecoder : Test
testModelDecoder =
    test "Make sure an expected JSON model string can be decoded" <|
        \() ->
            Expect.equal (Ok {id = 43, url = "hello"})
                <| JsonDecode.decodeString Asset.modelDecoder "{\"id\":43,\"url\":\"hello\"}"


testListDecoder : Test
testListDecoder =
    test "Make sure an expected JSON list string can be decoded" <|
        \() ->
            Expect.equal (Ok [{id = 43, url = "hello"},{id = 56, url = "hi"}])
                <| JsonDecode.decodeString Asset.listDecoder "[{\"id\":43,\"url\":\"hello\"},{\"id\":56,\"url\":\"hi\"}]"
