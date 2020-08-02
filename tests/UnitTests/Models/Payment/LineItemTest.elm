module UnitTests.Models.Payment.LineItemTest exposing (..)

import Expect
import Json.Decode as JsonDecode
import Models.Payment.LineItem as LineItem
import Test exposing (Test, test)


testModelDecoder : Test
testModelDecoder =
    test "Test minimal decode" <|
        \() ->
            Expect.equal (Ok { id = 342
                            , item_id = 32
                            , item_type = "subscription"
                            , amount = 12.43
                            })
                <| JsonDecode.decodeString LineItem.modelDecoder "{\"id\":342,\"item_id\":32,\"item_type\":\"subscription\",\"amount\":12.43}"
