module UnitTests.Models.Wiki.ArticleTest exposing (..)

import Athenia.Models.Wiki.Article as Article
import Expect
import Json.Decode as JsonDecode
import Test exposing (..)


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