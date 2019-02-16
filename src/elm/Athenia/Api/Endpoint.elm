module Athenia.Api.Endpoint exposing
    ( Endpoint, request
    , baseArticle, viewArticle, viewArticles
    , viewArticleIterations
    , login, signUp, refresh
    , user, userActivity, me
    )

import Athenia.Models.Wiki.Article as Article
import Http
import Url.Builder as Builder exposing (QueryParameter)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , withCredentials : Bool
    }
    -> Http.Request a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , withCredentials = config.withCredentials
        }



-- TYPES


{-| Get a URL to the API.

This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.

-}
type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Builder.crossOrigin "http://dev-api.projectathenia.com"
        ("v1" :: paths)
        queryParams
        |> Endpoint



-- System wide endpoints


baseArticle : Endpoint
baseArticle =
    url [ "articles" ] []

viewArticle : Int -> Endpoint
viewArticle articleId =
    url [ "articles", String.fromInt articleId ]
        [ (Builder.string "expand[createdBy]" "*")
        ]


viewArticles : Int -> Endpoint
viewArticles page =
    url [ "articles" ]
        <| if page /= 1 then
            [ (Builder.string "page" (String.fromInt page))
            , (Builder.string "expand[createdBy]" "*")
            ]
        else
            [ (Builder.string "expand[createdBy]" "*") ]

viewArticleIterations : Int -> Int -> Endpoint
viewArticleIterations articleId page =
    url [ "articles", String.fromInt articleId, "iterations" ]
        <| if page /= 1 then
            [ (Builder.string "page" (String.fromInt page))
            , (Builder.string "expand[createdBy]" "*")
            , (Builder.string "limit" "100")
            ]
        else
            [ (Builder.string "expand[createdBy]" "*")
            , (Builder.string "limit" "100")
            ]


me : Endpoint
me =
    url [ "users", "me" ] []


refresh : Endpoint
refresh =
    url [ "auth", "refresh" ] []


login : Endpoint
login =
    url [ "auth", "login" ] []


logout : Endpoint
logout =
    url [ "auth", "logout" ] []


signUp : Endpoint
signUp =
    url [ "auth", "sign-up" ] []


user : Int -> Endpoint
user userId =
    url [ "users", String.fromInt userId ] []


userActivity : Int -> Endpoint
userActivity userId =
    url [ "users", String.fromInt userId ]
        [ (Builder.string "expand[createdArticles]" "*")
        , (Builder.string "expand[createdIterations]" "*")
        ]
