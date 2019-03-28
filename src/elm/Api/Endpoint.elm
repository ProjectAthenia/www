module Api.Endpoint exposing
    ( Endpoint, request
    , baseArticle, viewArticle, viewArticles
    , viewArticleIterations
    , login, signUp, refresh
    , user, userActivity, me
    )

import Http
import Url.Builder as Builder exposing (QueryParameter)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { method : String
    , headers : List Http.Header
    , url : Endpoint
    , body : Http.Body
    , expect : Http.Expect msg
    , timeout : Maybe Float
    }
    -> Cmd msg
request config =
    Http.request
        { method = config.method
        , headers = config.headers
        , url = unwrap config.url
        , body = config.body
        , expect = config.expect
        , timeout = config.timeout
        , tracker = Nothing
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


url : String -> List String -> List QueryParameter -> Endpoint
url apiUrl paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Builder.crossOrigin apiUrl
        ("v1" :: paths)
        queryParams
        |> Endpoint



-- System wide endpoints


baseArticle : String -> Endpoint
baseArticle apiUrl =
    url apiUrl [ "articles" ] []

viewArticle : String -> Int -> Endpoint
viewArticle apiUrl articleId =
    url apiUrl [ "articles", String.fromInt articleId ]
        [ (Builder.string "expand[createdBy]" "*")
        ]


viewArticles : String -> Int -> Endpoint
viewArticles apiUrl page =
    url apiUrl [ "articles" ]
        <| if page /= 1 then
            [ (Builder.string "page" (String.fromInt page))
            , (Builder.string "expand[createdBy]" "*")
            ]
        else
            [ (Builder.string "expand[createdBy]" "*") ]

viewArticleIterations : String -> Int -> Int -> Endpoint
viewArticleIterations apiUrl articleId page =
    url apiUrl [ "articles", String.fromInt articleId, "iterations" ]
        <| if page /= 1 then
            [ (Builder.string "page" (String.fromInt page))
            , (Builder.string "expand[createdBy]" "*")
            , (Builder.string "limit" "100")
            ]
        else
            [ (Builder.string "expand[createdBy]" "*")
            , (Builder.string "limit" "100")
            ]


me : String -> Endpoint
me apiUrl =
    url apiUrl [ "users", "me" ] []


refresh : String -> Endpoint
refresh apiUrl =
    url apiUrl [ "auth", "refresh" ] []


login : String -> Endpoint
login apiUrl =
    url apiUrl [ "auth", "login" ] []


logout : String -> Endpoint
logout apiUrl =
    url apiUrl [ "auth", "logout" ] []


signUp : String -> Endpoint
signUp apiUrl =
    url apiUrl [ "auth", "sign-up" ] []


user : String -> Int -> Endpoint
user apiUrl userId =
    url apiUrl [ "users", String.fromInt userId ] []


userActivity : String -> Int -> Endpoint
userActivity apiUrl userId =
    url apiUrl [ "users", String.fromInt userId ]
        [ (Builder.string "expand[createdArticles]" "*")
        , (Builder.string "expand[createdIterations]" "*")
        ]
