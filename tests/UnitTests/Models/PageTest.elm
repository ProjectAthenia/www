module UnitTests.Models.PageTest exposing(..)

import Models.Page as Page
import Models.User.User as User
import Expect
import Json.Decode as JsonDecode
import Test exposing (..)


mockPageModel : Int -> Page.Model a
mockPageModel currentPage =
    { total = 12
    , data = []
    , current_page = currentPage
    , last_page = 2
    , per_page = 10
    }


testNextPageNumber : Test
testNextPageNumber =
    describe "Makes sure that the next page number is returned properly."
        [ test "Makes sure that next page number returns nothing when it is the last page" <|
            \() ->
                Expect.equal Nothing
                    <| Page.nextPageNumber (mockPageModel 2)
        , test "Makes sure that next page number returns the proper page number when the next should be 2" <|
            \() ->
                Expect.equal (Just 2)
                    <| Page.nextPageNumber (mockPageModel 1)
        ]


testPreviousPageNumber : Test
testPreviousPageNumber =
    describe "Makes sure that the previous page number is returned properly."
        [ test "Makes sure that previous page number returns nothing when it is the first page" <|
            \() ->
                Expect.equal Nothing
                    <| Page.previousPageNumber (mockPageModel 1)
        , test "Makes sure that previous page number returns the proper page number when the previous should be 1" <|
            \() ->
                Expect.equal (Just 1)
                    <| Page.previousPageNumber (mockPageModel 2)
        ]


testModelDecoder : Test
testModelDecoder =
    describe "Test various decoders to make sure that different strings can be decoded"
        [ test "Makes sure that a basic page with no data can be decoded" <|
            \() ->
                Expect.equal (Ok {total = 0, data = [], current_page = 1, last_page = 1, per_page = 10})
                    <| JsonDecode.decodeString (Page.modelDecoder User.listDecoder) "{\"total\":0,\"data\":[],\"current_page\":1,\"last_page\":1,\"per_page\":10}"
        , test "Makes sure that a page with a few roles can be decoded" <|
            \() ->
                Expect.equal (Ok {total = 1, data = [{id = 21, name = "Steve", email = "test@test.com", password = "", roles = [] }], current_page = 1, last_page = 1, per_page = 10})
                    <| JsonDecode.decodeString (Page.modelDecoder User.listDecoder) "{\"total\":1,\"data\":[{\"id\":21,\"name\":\"Steve\",\"email\":\"test@test.com\"}],\"current_page\":1,\"last_page\":1,\"per_page\":10}"
        ]
