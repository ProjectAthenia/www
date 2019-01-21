module UnitTests.Models.Wiki.ArticleTest exposing (..)

import Athenia.Models.Wiki.Article as Article
import Expect
import Json.Decode as JsonDecode
import Json.Encode as JsonEncode
import Test exposing (..)


testToCreateJson : Test
testToCreateJson =
    test "Make sure that we can transform a create model to a json string" <|
        \() ->
            let
                baseModel = Article.initCreateModel <|
                    { id = 123
                    , name = ""
                    , email = ""
                    , password = ""
                    , roles = []
                    }
                model =
                    { baseModel
                        | title = "An Article"
                    }
            in
                Expect.equal "{\"title\":\"An Article\",\"created_by_id\":123}"
                    <| JsonEncode.encode 0
                        <| Article.toCreateJson model



testModelDecoder : Test
testModelDecoder =
    describe "Tests multiple decode possibilities to make sure it works"
        [ test "Test minimal decode" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , title = "A Title"
                                , content = "Some Content"
                                , created_by = Nothing
                                , iterations = []
                                })
                    <| JsonDecode.decodeString Article.modelDecoder "{\"id\":342,\"title\":\"A Title\",\"content\":\"Some Content\"}"
        , test "Test decode with created by set" <|
            \() ->
                Expect.equal (Ok { id = 342
                                , title = "A Title"
                                , content = "Some Content"
                                , created_by =
                                    Just { id = 53
                                        , name = "Barry Manilow"
                                        , email = "butts@butts.com"
                                        , password  = ""
                                        , roles = []
                                        }
                                , iterations =
                                    [ { id = 342
                                      , content = "Some Content"
                                      , created_by = Nothing
                                      }
                                    ]
                                })
                    <| JsonDecode.decodeString Article.modelDecoder "{\"id\":342,\"title\":\"A Title\",\"content\":\"Some Content\",\"created_by\":{\"id\":53,\"name\":\"Barry Manilow\",\"email\":\"butts@butts.com\"},\"iterations\":[{\"id\":342,\"content\":\"Some Content\"}]}"
        ]