module UnitTests.Utilities.ConfigurationTest exposing (..)

import Dict
import Expect
import Json.Decode as JsonDecode
import Test exposing (..)
import Utilities.Configuration as Configuration


testDecoder: Test
testDecoder =
    test "Makes sure that a configuration model can be decoded properly" <|
        \() ->
            Expect.equal (Ok  (Dict.fromList [("Hello", "Something"),("Hi", "Test")]))
                <| JsonDecode.decodeString Configuration.decoder "{\"Hello\":\"Something\",\"Hi\":\"Test\"}"


testFetchConfigVariable: Test
testFetchConfigVariable =
    describe "Make sure that we can pull config variables properly"
        [ test "should pull a config variable properly" <|
            \() ->
                Expect.equal (Just "Test")
                    <| Configuration.fetchConfigVariable (Dict.fromList [("Hello", "Something"),("Hi", "Test")]) "Hi"
        , test "should reutnr nothing when a config variable is not set" <|
            \() ->
                Expect.equal Nothing
                    <| Configuration.fetchConfigVariable (Dict.fromList [("Hello", "Something"),("Hi", "Test")]) "Hola"
        ]
