port module Athenia.Api exposing
    (Token
    , application
    , decodeErrors
    , delete, get, post, put
    , login, logout, signUp, refresh
    , settings, me
    , getArticle, viewArticles, createArticle
    , storeCredWith
    , viewerChanges
    )

{-| This module is responsible for communicating to the Conduit API.

It exposes an opaque Endpoint type which is guaranteed to point to the correct URL.

-}

import Athenia.Api.Endpoint as Endpoint
import Athenia.Models.Error as Error
import Athenia.Models.User.User as User
import Athenia.Models.Wiki.Article as Article
import Browser
import Browser.Navigation as Nav
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Url exposing (Url)



-- CRED


{-| The authentication credentials for the Viewer (that is, the currently logged-in user.)

This includes:

  - The cred's Username
  - The cred's authentication token

By design, there is no way to access the token directly as a String.
It can be encoded for persistence, and it can be added to a header
to a HttpBuilder for a request, but that's it.

This token should never be rendered to the end user, and with this API, it
can't be!

-}
type Token
    = Token String


authHeader : Token -> Http.Header
authHeader (Token str) =
    Http.header "Authorization" ("Bearer " ++ str)


{-| It's important that this is never exposed!

We epxose `login` and `application` instead, so we can be certain that if anyone
ever has access to a `Cred` value, it came from either the login API endpoint
or was passed in via flags.

-}
tokenDecoder : Decoder Token
tokenDecoder =
    Decode.succeed Token
        |> required "token" Decode.string


lastRefreshDecoder : Decoder Int
lastRefreshDecoder =
    field "last_refresh" Decode.int



-- PERSISTENCE


decode : Decoder (Token -> Int -> viewer) -> Value -> Result Decode.Error viewer
decode decoder value =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue Decode.string value
        |> Result.andThen (\str -> Decode.decodeString (Decode.field "user" (decoderFromCred decoder)) str)


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Maybe viewer -> msg) -> Decoder (Token -> Int -> viewer) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder (Token -> Int -> viewer) -> Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue (storageDecoder viewerDecoder) val
        |> Result.toMaybe


storeCredWith : Token -> Int -> User.Model -> Cmd msg
storeCredWith (Token token) lastRefresh user =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "token", Encode.string token )
                        , ( "last_refresh", Encode.int lastRefresh )
                        , ( "model", User.cacheEncoder user )
                        ]
                  )
                ]
    in
    storeCache (Just json)


logout : Cmd msg
logout =
    storeCache Nothing


port storeCache : Maybe Value -> Cmd msg



-- SERIALIZATION
-- APPLICATION


application :
    Decoder (Token -> Int -> viewer)
    ->
        { init : Maybe viewer -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application viewerDecoder config =
    let
        init flags url navKey =
            let
                maybeViewer =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString (storageDecoder viewerDecoder))
                        |> Result.toMaybe
            in
            config.init maybeViewer url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


storageDecoder : Decoder (Token -> Int -> viewer) -> Decoder viewer
storageDecoder viewerDecoder =
    Decode.field "user" (decoderFromCred viewerDecoder)



-- HTTP


get : Endpoint.Endpoint -> Maybe Token -> Decoder a -> Http.Request a
get url maybeToken decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson decoder
        , headers =
            case maybeToken of
                Just cred ->
                    [ authHeader cred ]

                Nothing ->
                    []
        , body = Http.emptyBody
        , timeout = Nothing
        , withCredentials = False
        }


put : Endpoint.Endpoint -> Token -> Body -> Decoder a -> Http.Request a
put url token body decoder =
    Endpoint.request
        { method = "PUT"
        , url = url
        , expect = Http.expectJson decoder
        , headers = [ authHeader token ]
        , body = body
        , timeout = Nothing
        , withCredentials = False
        }


post : Endpoint.Endpoint -> Maybe Token -> Body -> Decoder a -> Http.Request a
post url maybeToken body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson decoder
        , headers =
            case maybeToken of
                Just token ->
                    [ authHeader token ]

                Nothing ->
                    []
        , body = body
        , timeout = Nothing
        , withCredentials = False
        }


delete : Endpoint.Endpoint -> Token -> Body -> Decoder a -> Http.Request a
delete url token body decoder =
    Endpoint.request
        { method = "DELETE"
        , url = url
        , expect = Http.expectJson decoder
        , headers = [ authHeader token ]
        , body = body
        , timeout = Nothing
        , withCredentials = False
        }


login : Http.Body -> Http.Request Token
login body =
    post Endpoint.login Nothing body tokenDecoder


signUp : Http.Body -> Http.Request Token
signUp body =
    post Endpoint.signUp Nothing body tokenDecoder


refresh : Token -> Http.Request Token
refresh token =
    post Endpoint.refresh (Just token) Http.emptyBody tokenDecoder


settings : Token -> Int -> Http.Body -> Http.Request User.Model
settings token userId body =
    put (Endpoint.user userId) token body User.modelDecoder


me : Token -> Http.Request User.Model
me token =
    get Endpoint.me (Just token) User.modelDecoder


createArticle : Token -> Article.CreateModel -> Http.Request Article.Model
createArticle token article =
    post Endpoint.baseArticle
        (Just token)
        (Http.jsonBody (Article.toCreateJson article))
        Article.modelDecoder


getArticle : Token -> Int -> Http.Request Article.Model
getArticle token articleId =
    get (Endpoint.viewArticle articleId) (Just token) Article.modelDecoder


viewArticles : Token -> Int -> Http.Request Article.ArticlePage
viewArticles token page =
    get (Endpoint.viewArticles page) (Just token) Article.pageDecoder


decoderFromCred : Decoder (Token -> Int -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map3 (\fromCred cred -> fromCred cred)
        decoder
        tokenDecoder
        lastRefreshDecoder




{-| Many API endpoints include an "errors" field in their BadStatus responses.
-}
decodeErrors : Http.Error -> Error.Model
decodeErrors error =
    case error of
        Http.BadStatus response ->
            response.body
                |> decodeString Error.decoder
                |> Result.withDefault Error.unknownErrorResponse

        err ->
            Error.unknownErrorResponse