module UnitTests.Models.Payment.PaymentTest exposing (..)

import Expect
import Json.Decode as JsonDecode
import Models.Payment.Payment as Payment
import Test exposing (Test, describe, test)
import Time exposing (millisToPosix)


testModelDecoder : Test
testModelDecoder =
    describe "Test various decodes"
        [ test "Test minimal decode" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , amount = 12.43
                                , refunded_at = Nothing
                                , payment_method = Nothing
                                , line_items = []
                                })
                    <| JsonDecode.decodeString Payment.modelDecoder "{\"id\":342,\"amount\":12.43}"
        , test "Test decode with a refunded at timestamp" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , amount = 12.43
                                , refunded_at = Just <| millisToPosix 1000
                                , payment_method = Nothing
                                , line_items = []
                                })
                    <| JsonDecode.decodeString Payment.modelDecoder "{\"id\":342,\"amount\":12.43,\"refunded_at\":\"1970-01-01T00:00:01+00:00\"}"
        , test "Test decode with a payment method" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , amount = 12.43
                                , refunded_at = Nothing
                                , payment_method = Just
                                    { id = 342
                                    , payment_method_key = "payment_method_key"
                                    , payment_method_type = "payment_method_type"
                                    , identifier = Just "identifier"
                                    }
                                , line_items = []
                                })
                    <| JsonDecode.decodeString Payment.modelDecoder "{\"id\":342,\"amount\":12.43,\"payment_method\":{\"id\":342,\"payment_method_key\":\"payment_method_key\",\"payment_method_type\":\"payment_method_type\",\"identifier\":\"identifier\"}}"
        , test "Test decode with a line item" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , amount = 12.43
                                , refunded_at = Nothing
                                , payment_method = Nothing
                                , line_items =
                                    [ { id = 342
                                      , item_id = 32
                                      , item_type = "subscription"
                                      , amount = 12.43
                                      }
                                    ]
                                })
                    <| JsonDecode.decodeString Payment.modelDecoder "{\"id\":342,\"amount\":12.43,\"line_items\":[{\"id\":342,\"item_id\":32,\"item_type\":\"subscription\",\"amount\":12.43}]}"
        ]
