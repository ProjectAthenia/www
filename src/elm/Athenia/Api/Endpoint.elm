module Athenia.Api.Endpoint exposing
    ( Endpoint, request
    , article, articles
    , login, signUp
    , user
    )

import Http
import Url.Builder exposing (QueryParameter)


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
    Url.Builder.crossOrigin "http://dev-api.projectathenia.com"
        ("v1" :: paths)
        queryParams
        |> Endpoint



-- System wide endpoints


article : Int -> Endpoint
article articleId =
    url [ "articles", String.fromInt articleId ] []


articles : List QueryParameter -> Endpoint
articles params =
    url [ "articles" ] params


login : Endpoint
login =
    url [ "users", "login" ] []


signUp : Endpoint
signUp =
    url [ "sign-up" ] []


user : Int -> Endpoint
user userId =
    url [ "users", String.fromInt userId ]
