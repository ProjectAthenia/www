module UnitTests.Models.MembershipPlan.SubscriptionTest exposing (..)

import Expect
import Json.Decode as JsonDecode
import Json.Encode as JsonEncode
import Models.MembershipPlan.Subscription as Subscription
import Test exposing (..)
import Time exposing (..)


testCompareExpiration : Test
testCompareExpiration =
    let
        lifetime =
            { id = 342
            , last_renewed_at = millisToPosix 1000
            , subscribed_at = millisToPosix 2000
            , expires_at = Nothing
            , canceled_at = Nothing
            , recurring = False
            , membership_plan_rate = Just
                { id = 2
                , cost = 4.00
                , membership_plan =
                    { id = 453
                    , name = "A membership plan"
                    , duration = "yearly"
                    , current_cost = 10.00
                    , current_rate_id = 5
                    }
                }
            , payment_method = Just { id = 563
                               , payment_method_key = "test_key"
                               , payment_method_type = "stripe"
                               , identifier = Nothing
                               }
            }
        later =
            { id = 342
            , last_renewed_at = millisToPosix 1000
            , subscribed_at = millisToPosix 2000
            , expires_at = Just (millisToPosix 5000)
            , canceled_at = Nothing
            , recurring = False
            , membership_plan_rate = Just
                { id = 2
                , cost = 4.00
                , membership_plan =
                    { id = 453
                    , name = "A membership plan"
                    , duration = "yearly"
                    , current_cost = 10.00
                    , current_rate_id = 5
                    }
                }
            , payment_method = Just { id = 563
                               , payment_method_key = "test_key"
                               , payment_method_type = "stripe"
                               , identifier = Nothing
                               }
            }
        early =
            { id = 342
            , last_renewed_at = millisToPosix 1000
            , subscribed_at = millisToPosix 2000
            , expires_at = Just (millisToPosix 1000)
            , canceled_at = Nothing
            , recurring = False
            , membership_plan_rate = Just
                { id = 2
                , cost = 4.00
                , membership_plan =
                    { id = 453
                    , name = "A membership plan"
                    , duration = "yearly"
                    , current_cost = 10.00
                    , current_rate_id = 5
                    }
                }
            , payment_method = Just { id = 563
                               , payment_method_key = "test_key"
                               , payment_method_type = "stripe"
                               , identifier = Nothing
                               }
            }
    in
    describe "Make sure that compare expiration function works properly"
        [ test "Test to make sure that a non-expiring subscription is lt a later one" <|
            \() ->
                Expect.equal LT
                    <| Subscription.compareExpiration lifetime later
        , test "Test to make sure that a later subscription is greater than non-expiring subscription" <|
            \() ->
                Expect.equal GT
                    <| Subscription.compareExpiration later lifetime
        , test "Test to make sure that a later subscription is greater than a early subscription" <|
            \() ->
                Expect.equal GT
                    <| Subscription.compareExpiration later early
        , test "Test to make sure that a early subscription is greater than a later subscription" <|
            \() ->
                Expect.equal LT
                    <| Subscription.compareExpiration early later
        ]


testIsActive : Test
testIsActive =
    describe "Test various is active responses based on various subscriptions"
        [ test "Test that a lifetime subscription is always active" <|
            \() ->
                Expect.true ""
                    <| Subscription.isActive (millisToPosix 2000)
                        { id = 342
                        , last_renewed_at = millisToPosix 1000
                        , subscribed_at = millisToPosix 2000
                        , expires_at = Nothing
                        , canceled_at = Nothing
                        , recurring = False
                        , membership_plan_rate = Just
                            { id = 2
                            , cost = 4.00
                            , membership_plan =
                                { id = 453
                                , name = "A membership plan"
                                , duration = "yearly"
                                , current_cost = 10.00
                                , current_rate_id = 5
                                }
                            }
                        , payment_method = Just { id = 563
                                           , payment_method_key = "test_key"
                                           , payment_method_type = "stripe"
                                           , identifier = Nothing
                                           }
                        }
        , test "Test that a subscription is active that has not expired yet" <|
            \() ->
                Expect.true ""
                    <| Subscription.isActive (millisToPosix 2000)
                        { id = 342
                        , last_renewed_at = millisToPosix 1000
                        , subscribed_at = millisToPosix 2000
                        , expires_at = Just (millisToPosix 5000)
                        , canceled_at = Nothing
                        , recurring = False
                        , membership_plan_rate = Just
                            { id = 2
                            , cost = 4.00
                            , membership_plan =
                                { id = 453
                                , name = "A membership plan"
                                , duration = "yearly"
                                , current_cost = 10.00
                                , current_rate_id = 5
                                }
                            }
                        , payment_method = Just
                            { id = 563
                            , payment_method_key = "test_key"
                            , payment_method_type = "stripe"
                            , identifier = Nothing
                            }
                        }
        , test "Test that a subscription is not active that has expired" <|
            \() ->
                Expect.false ""
                    <| Subscription.isActive (millisToPosix 2000)
                        { id = 342
                        , last_renewed_at = millisToPosix 1000
                        , subscribed_at = millisToPosix 2000
                        , expires_at = Just (millisToPosix 1000)
                        , canceled_at = Nothing
                        , recurring = False
                        , membership_plan_rate = Just
                            { id = 2
                            , cost = 4.00
                            , membership_plan =
                                { id = 453
                                , name = "A membership plan"
                                , duration = "yearly"
                                , current_cost = 10.00
                                , current_rate_id = 5
                                }
                            }
                        , payment_method = Just
                            { id = 563
                            , payment_method_key = "test_key"
                            , payment_method_type = "stripe"
                            , identifier = Nothing
                            }
                        }
        ]

testModelDecoder : Test
testModelDecoder =
    describe "Test various model decoders"
        [ test "Test base decode" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , last_renewed_at = millisToPosix 1000
                                , subscribed_at = millisToPosix 2000
                                , expires_at = Just (millisToPosix 4000)
                                , canceled_at = Nothing
                                , recurring = False
                                , membership_plan_rate = Just
                                    { id = 2
                                    , cost = 4.00
                                    , membership_plan =
                                        { id = 453
                                        , name = "A membership plan"
                                        , duration = "yearly"
                                        , current_cost = 10.00
                                        , current_rate_id = 5
                                        }
                                    }
                                , payment_method = Just { id = 563
                                                   , payment_method_key = "test_key"
                                                   , payment_method_type = "stripe"
                                                   , identifier = Nothing
                                                   }
                                })
                    <| JsonDecode.decodeString Subscription.modelDecoder "{\"id\":342,\"last_renewed_at\":\"1970-01-01T00:00:01+00:00\",\"subscribed_at\":\"1970-01-01T00:00:02+00:00\",\"expires_at\":\"1970-01-01T00:00:04+00:00\",\"canceled_at\":null,\"recurring\":false,\"membership_plan_rate\":{\"id\":2,\"cost\":\"4.00\",\"membership_plan\":{\"id\":453,\"name\":\"A membership plan\",\"duration\":\"yearly\",\"current_cost\":\"10.00\",\"current_rate_id\":5}},\"payment_method\":{\"id\":563,\"payment_method_key\":\"test_key\",\"payment_method_type\":\"stripe\"}}"
        , test "Test complete decode" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , last_renewed_at = millisToPosix 1000
                                , subscribed_at = millisToPosix 2000
                                , expires_at = Just (millisToPosix 4000)
                                , canceled_at = Just (millisToPosix 5000)
                                , recurring = False
                                , membership_plan_rate = Just
                                    { id = 2
                                    , cost = 4.00
                                    , membership_plan =
                                        { id = 453
                                        , name = "A membership plan"
                                        , duration = "yearly"
                                        , current_cost = 10.00
                                        , current_rate_id = 5
                                        }
                                    }
                                , payment_method = Just { id = 563
                                                   , payment_method_key = "test_key"
                                                   , payment_method_type = "stripe"
                                                   , identifier = Nothing
                                                   }
                                })
                    <| JsonDecode.decodeString Subscription.modelDecoder "{\"id\":342,\"last_renewed_at\":\"1970-01-01T00:00:01+00:00\",\"subscribed_at\":\"1970-01-01T00:00:02+00:00\",\"expires_at\":\"1970-01-01T00:00:04+00:00\",\"canceled_at\":\"1970-01-01T00:00:05+00:00\",\"recurring\":false,\"membership_plan_rate\":{\"id\":2,\"cost\":\"4.00\",\"membership_plan\":{\"id\":453,\"name\":\"A membership plan\",\"duration\":\"yearly\",\"current_cost\":\"10.00\",\"current_rate_id\":5}},\"payment_method\":{\"id\":563,\"payment_method_key\":\"test_key\",\"payment_method_type\":\"stripe\"}}"
        ]


testToCreateJson : Test
testToCreateJson =
    test "Makes sure that an empty user can be encoded" <|
        \() ->
            let
                model = { recurring = True
                        , membership_plan_rate_id = 432
                        , payment_method_id = 76
                        }
            in
                Expect.equal "{\"recurring\":true,\"membership_plan_rate_id\":432,\"payment_method_id\":76}"
                    <| JsonEncode.encode 0
                        <| Subscription.toCreateJson model
