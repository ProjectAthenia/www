module UnitTests.Models.ErrorTest exposing(..)

import Models.Error as Error
import Expect
import Json.Decode as JsonDecode
import Test exposing (..)


testDecoder : Test
testDecoder =
    describe "Tests various strings to make sure that different types of error models can be decoded."
        [ test "Make sure a JSON string can be decoded without a list of errors." <|
            \() ->
                Expect.equal (Ok {message = "hello", errors = []})
                    <| JsonDecode.decodeString Error.decoder "{\"message\":\"hello\"}"
        ,  test "Make sure a JSON string can be decoded with a list of errors." <|
            \() ->
                Expect.equal (Ok {message = "hello", errors = [("test", ["Something", "Else"])]})
                    <| JsonDecode.decodeString Error.decoder "{\"message\":\"hello\",\"errors\":{\"test\":[\"Something\",\"Else\"]}}"
        ]
