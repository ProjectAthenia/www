module UnitTests.Models.User.UserTest exposing (..)

import Models.Role as Role
import Models.User.User as User
import Expect
import Json.Decode as JsonDecode
import Json.Encode as JsonEncode
import Test exposing (..)
import Time exposing (..)


mockUser : String -> String -> String -> List Role.Model -> User.Model
mockUser name email password roles =
    { id = 543
    , name = name
    , email = email
    , password = password
    , stripe_customer_key = Nothing
    , roles = roles
    , payment_methods = []
    , subscriptions = []
    }


testGetActiveSubscriptions : Test
testGetActiveSubscriptions =
    test "Makes sure that we can get the active subscriptions properly" <|
        \() ->
            let
                model = mockUser "" "" "" []
                readyModel =
                    { model
                        | subscriptions =
                            [ { id = 342
                              , last_renewed_at = millisToPosix 1000
                              , subscribed_at = millisToPosix 2000
                              , expires_at = Nothing
                              , canceled_at = Nothing
                              , recurring = False
                              , membership_plan = { id = 453
                                                  , name = "A membership plan"
                                                  , duration = "yearly"
                                                  , current_cost = 10.00
                                                  , current_rate_id = 5
                                                  }
                              , membership_plan_rate = { id = 2
                                                       , cost = 4.00
                                                       }
                              , payment_method = { id = 563
                                                 , payment_method_key = "test_key"
                                                 , payment_method_type = "stripe"
                                                 , identifier = Nothing
                                                 }
                              }
                            , { id = 435
                              , last_renewed_at = millisToPosix 1000
                              , subscribed_at = millisToPosix 2000
                              , expires_at = Just (millisToPosix 5000)
                              , canceled_at = Nothing
                              , recurring = False
                              , membership_plan = { id = 453
                                                  , name = "A membership plan"
                                                  , duration = "yearly"
                                                  , current_cost = 10.00
                                                  , current_rate_id = 5
                                                  }
                              , membership_plan_rate = { id = 2
                                                       , cost = 4.00
                                                       }
                              , payment_method = { id = 563
                                                 , payment_method_key = "test_key"
                                                 , payment_method_type = "stripe"
                                                 , identifier = Nothing
                                                 }
                              }
                            , { id = 6342
                              , last_renewed_at = millisToPosix 1000
                              , subscribed_at = millisToPosix 2000
                              , expires_at = Just (millisToPosix 1000)
                              , canceled_at = Nothing
                              , recurring = False
                              , membership_plan = { id = 453
                                                  , name = "A membership plan"
                                                  , duration = "yearly"
                                                  , current_cost = 10.00
                                                  , current_rate_id = 5
                                                  }
                              , membership_plan_rate = { id = 2
                                                       , cost = 4.00
                                                       }
                              , payment_method = { id = 563
                                                 , payment_method_key = "test_key"
                                                 , payment_method_type = "stripe"
                                                 , identifier = Nothing
                                                 }
                              }
                            ]
                    }
            in
            Expect.equal
                [ { id = 342
                  , last_renewed_at = millisToPosix 1000
                  , subscribed_at = millisToPosix 2000
                  , expires_at = Nothing
                  , canceled_at = Nothing
                  , recurring = False
                  , membership_plan = { id = 453
                                      , name = "A membership plan"
                                      , duration = "yearly"
                                      , current_cost = 10.00
                                      , current_rate_id = 5
                                      }
                  , membership_plan_rate = { id = 2
                                           , cost = 4.00
                                           }
                  , payment_method = { id = 563
                                     , payment_method_key = "test_key"
                                     , payment_method_type = "stripe"
                                     , identifier = Nothing
                                     }
                  }
                , { id = 435
                  , last_renewed_at = millisToPosix 1000
                  , subscribed_at = millisToPosix 2000
                  , expires_at = Just (millisToPosix 5000)
                  , canceled_at = Nothing
                  , recurring = False
                  , membership_plan = { id = 453
                                      , name = "A membership plan"
                                      , duration = "yearly"
                                      , current_cost = 10.00
                                      , current_rate_id = 5
                                      }
                  , membership_plan_rate = { id = 2
                                           , cost = 4.00
                                           }
                  , payment_method = { id = 563
                                     , payment_method_key = "test_key"
                                     , payment_method_type = "stripe"
                                     , identifier = Nothing
                                     }
                  }
                ]
                <| User.getActiveSubscriptions (millisToPosix 2000) readyModel


testGetCurrentSubscription : Test
testGetCurrentSubscription =
    let
        model = mockUser "" "" "" []
        lifetime =
            { id = 342
            , last_renewed_at = millisToPosix 1000
            , subscribed_at = millisToPosix 2000
            , expires_at = Nothing
            , canceled_at = Nothing
            , recurring = False
            , membership_plan = { id = 453
                                , name = "A membership plan"
                                , duration = "yearly"
                                , current_cost = 10.00
                                , current_rate_id = 5
                                }
            , membership_plan_rate = { id = 2
                                     , cost = 4.00
                                     }
            , payment_method = { id = 563
                               , payment_method_key = "test_key"
                               , payment_method_type = "stripe"
                               , identifier = Nothing
                               }
            }
        early =
            { id = 342
            , last_renewed_at = millisToPosix 1000
            , subscribed_at = millisToPosix 2000
            , expires_at = Just (millisToPosix 3000)
            , canceled_at = Nothing
            , recurring = False
            , membership_plan = { id = 453
                                , name = "A membership plan"
                                , duration = "yearly"
                                , current_cost = 10.00
                                , current_rate_id = 5
                                }
            , membership_plan_rate = { id = 2
                                     , cost = 4.00
                                     }
            , payment_method = { id = 563
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
            , membership_plan = { id = 453
                                , name = "A membership plan"
                                , duration = "yearly"
                                , current_cost = 10.00
                                , current_rate_id = 5
                                }
            , membership_plan_rate = { id = 2
                                     , cost = 4.00
                                     }
            , payment_method = { id = 563
                               , payment_method_key = "test_key"
                               , payment_method_type = "stripe"
                               , identifier = Nothing
                               }
            }
    in
    describe "Makes sure that we can get the current subscription properly"
        [ test "Makes sure that we can get the lifetime one first" <|
            \() ->
                let
                    readyUser =
                        { model
                            | subscriptions =
                                [lifetime, early, later]
                        }
                in
                Expect.equal (Just lifetime)
                    <| User.getCurrentSubscription (millisToPosix 1000) readyUser
        , test "Makes sure that we can get the earlier one" <|
            \() ->
                let
                    readyUser =
                        { model
                            | subscriptions =
                                [early, later]
                        }
                in
                Expect.equal (Just early)
                    <| User.getCurrentSubscription (millisToPosix 1000) readyUser
        , test "Makes sure that we can get the later one" <|
            \() ->
                let
                    readyUser =
                        { model
                            | subscriptions =
                                [early, later]
                        }
                in
                Expect.equal (Just later)
                    <| User.getCurrentSubscription (millisToPosix 4000) readyUser
        , test "Makes sure that nothing is returned" <|
            \() ->
                let
                    readyUser =
                        { model
                            | subscriptions =
                                [early]
                        }
                in
                Expect.equal Nothing
                    <| User.getCurrentSubscription (millisToPosix 4000) readyUser
        ]


testToJson : Test
testToJson =
    describe "Makes sure that various user models can be encoding to JSON properly"
        [ test "Makes sure that an empty user can be encoded" <|
            \() ->
                let
                    model = mockUser "" "" "" []
                in
                    Expect.equal "{}"
                        <| JsonEncode.encode 0
                            <| User.toJson model
        , test "Makes sure that an login user can be encoded" <|
            \() ->
                let
                    model = mockUser "" "test@test.com" "secret" []
                in
                    Expect.equal "{\"email\":\"test@test.com\",\"password\":\"secret\"}"
                        <| JsonEncode.encode 0
                            <| User.toJson model
        , test "Makes sure that we can encode a full user properly" <|
            \() ->
                let
                    model = mockUser "Hello" "test@test.com" "secret" []
                in
                    Expect.equal "{\"email\":\"test@test.com\",\"password\":\"secret\",\"name\":\"Hello\"}"
                        <| JsonEncode.encode 0
                            <| User.toJson model
        ]


testCacheEncoder : Test
testCacheEncoder =
   test "Makes sure that the cache encoder works properly" <|
        \() ->
            let
                model = mockUser "Hello" "test@test.com" "secret" []
            in
                Expect.equal "{\"id\":543,\"name\":\"Hello\",\"email\":\"test@test.com\",\"roles\":[]}"
                    <| JsonEncode.encode 0
                        <| User.cacheEncoder model


testModelDecoder : Test
testModelDecoder =
    describe "Tests multiple decode possibilities to make sure it works"
        [ test "Test minimal decode" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , name = "Steve"
                                , email = "test@test.com"
                                , password = ""
                                , stripe_customer_key = Nothing
                                , roles = []
                                , payment_methods = []
                                , subscriptions = []
                                })
                    <| JsonDecode.decodeString User.modelDecoder "{\"id\":342,\"name\":\"Steve\",\"email\":\"test@test.com\"}"
        , test "Test decode with roles" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , name = "Steve"
                                , email = "test@test.com"
                                , password = ""
                                , stripe_customer_key = Nothing
                                , roles =
                                  [ { id = 2
                                    , name = "A Role"
                                    }
                                  , { id = 6
                                    , name = "A Different Role"
                                    }
                                  ]
                                , payment_methods = []
                                , subscriptions = []
                                })
                    <| JsonDecode.decodeString User.modelDecoder "{\"id\":342,\"name\":\"Steve\",\"email\":\"test@test.com\",\"roles\":[{\"id\":2,\"name\":\"A Role\"},{\"id\":6,\"name\":\"A Different Role\"}]}"
        , test "Test decode with payment methods" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , name = "Steve"
                                , email = "test@test.com"
                                , password = ""
                                , stripe_customer_key = Just "test_key"
                                , roles = []
                                , payment_methods =
                                  [ { id = 4354
                                    , payment_method_key = "Hi"
                                    , payment_method_type = "bye"
                                    , identifier = Nothing
                                    }
                                  ]
                                , subscriptions = []
                                })
                    <| JsonDecode.decodeString User.modelDecoder "{\"id\":342,\"name\":\"Steve\",\"email\":\"test@test.com\",\"stripe_customer_key\":\"test_key\",\"payment_methods\":[{\"id\":4354,\"payment_method_key\":\"Hi\",\"payment_method_type\":\"bye\"}]}"
        ]
