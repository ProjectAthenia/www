module UnitTests.Models.User.UserTest exposing (..)

import Models.Role as Role
import Models.User.User as User
import Expect
import Json.Decode as JsonDecode
import Json.Encode as JsonEncode
import Test exposing (..)


mockUser : String -> String -> String -> List Role.Model -> User.Model
mockUser name email password roles =
    { id = 543
    , name = name
    , email = email
    , password = password
    , stripe_customer_key = Nothing
    , roles = roles
    , payment_methods = []
    }


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
                                })
                    <| JsonDecode.decodeString User.modelDecoder "{\"id\":342,\"name\":\"Steve\",\"email\":\"test@test.com\",\"stripe_customer_key\":\"test_key\",\"payment_methods\":[{\"id\":4354,\"payment_method_key\":\"Hi\",\"payment_method_type\":\"bye\"}]}"
        ]
