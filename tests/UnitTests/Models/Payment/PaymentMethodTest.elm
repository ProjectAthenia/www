module UnitTests.Models.Payment.PaymentMethodTest exposing (..)

import Expect
import Json.Decode as JsonDecode
import Json.Encode as JsonEncode
import Models.Payment.PaymentMethod as PaymentMethod
import Test exposing (..)


testToCreateModel: Test
testToCreateModel =
    test "Makes sure that the payment method model can be encoding to JSON properly" <|
        \() ->
            let
                model = { payment_method_key = "payment_method_key"
                        , payment_method_type = "payment_method_type"
                        }
            in
                Expect.equal "{\"payment_method_key\":\"payment_method_key\",\"payment_method_type\":\"payment_method_type\"}"
                    <| JsonEncode.encode 0
                        <| PaymentMethod.toCreateJson model

testModelDecoder : Test
testModelDecoder =
    test "Test minimal decode" <|
        \() ->
            Expect.equal (Ok { id = 342
                            , payment_method_key = "payment_method_key"
                            , payment_method_type = "payment_method_type"
                            , identifier = Just "identifier"
                            })
                <| JsonDecode.decodeString PaymentMethod.modelDecoder "{\"id\":342,\"payment_method_key\":\"payment_method_key\",\"payment_method_type\":\"payment_method_type\",\"identifier\":\"identifier\"}"

