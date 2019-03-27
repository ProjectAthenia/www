port module Api exposing
    ( Token
    , Error(..)
    , application
    , unwrapToken
    , decodeErrors
    , delete, get, post, put
    , login, logout, signUp, refresh
    , settings, me
    , getArticle, viewArticles, createArticle
    , viewArticleIterations
    , storeCredWith
    , viewerChanges
    )

{-| This module is responsible for communicating to the Conduit API.

It exposes an opaque Endpoint type which is guaranteed to point to the correct URL.

-}

import Api.Endpoint as Endpoint
import Models.Error as Error
import Models.User.User as User
import Models.Wiki.Article as Article
import Models.Wiki.Iteration as Iteration
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


unwrapToken : Token -> String
unwrapToken (Token str) =
    str


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


get : Endpoint.Endpoint -> Maybe Token -> Decoder a -> (Result Error a -> msg) -> Cmd msg
get url maybeToken decoder toMsg =
    Endpoint.request
        { method = "GET"
        , headers =
            case maybeToken of
                Just cred ->
                    [ authHeader cred ]

                Nothing ->
                    []
        , url = url
        , expect = expectJson toMsg decoder
        , body = Http.emptyBody
        , timeout = Nothing
        }


put : Endpoint.Endpoint -> Token -> Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
put url token body decoder toMsg =
    Endpoint.request
        { method = "PUT"
        , headers = [ authHeader token ]
        , url = url
        , expect = expectJson toMsg decoder
        , body = body
        , timeout = Nothing
        }


post : Endpoint.Endpoint -> Maybe Token -> Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
post url maybeToken body decoder toMsg =
    Endpoint.request
        { method = "POST"
        , headers =
            case maybeToken of
                Just token ->
                    [ authHeader token ]

                Nothing ->
                    []
        , url = url
        , expect = expectJson toMsg decoder
        , body = body
        , timeout = Nothing
        }


delete : Endpoint.Endpoint -> Token -> Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
delete url token body decoder toMsg =
    Endpoint.request
        { method = "DELETE"
        , headers = [ authHeader token ]
        , url = url
        , expect = expectJson toMsg decoder
        , body = body
        , timeout = Nothing
        }


login : Http.Body -> (Result Error Token -> msg) -> Cmd msg
login body toMsg =
    post Endpoint.login Nothing body tokenDecoder toMsg


signUp : Http.Body -> (Result Error Token -> msg) -> Cmd msg
signUp body toMsg =
    post Endpoint.signUp Nothing body tokenDecoder toMsg


refresh : Token -> (Result Error Token -> msg) -> Cmd msg
refresh token toMsg =
    post Endpoint.refresh (Just token) Http.emptyBody tokenDecoder toMsg


settings : Token -> Int -> Http.Body -> (Result Error User.Model -> msg) -> Cmd msg
settings token userId body toMsg =
    put (Endpoint.user userId) token body User.modelDecoder toMsg


me : Token -> (Result Error User.Model -> msg) -> Cmd msg
me token toMsg =
    get Endpoint.me (Just token) User.modelDecoder toMsg


createArticle : Token -> Article.CreateModel -> (Result Error Article.Model -> msg) -> Cmd msg
createArticle token article  toMsg =
    post Endpoint.baseArticle
        (Just token)
        (Http.jsonBody (Article.toCreateJson article))
        Article.modelDecoder
        toMsg


getArticle : Token -> Int -> (Result Error Article.Model -> msg) -> Cmd msg
getArticle token articleId toMsg =
    get (Endpoint.viewArticle articleId) (Just token) Article.modelDecoder toMsg


viewArticles : Token -> Int -> (Result Error Article.Page -> msg) -> Cmd msg
viewArticles token page toMsg =
    get (Endpoint.viewArticles page) (Just token) Article.pageDecoder toMsg


viewArticleIterations : Token -> Int -> Int -> (Result Error Iteration.Page -> msg) -> Cmd msg
viewArticleIterations token articleId page toMsg =
    get (Endpoint.viewArticleIterations articleId page) (Just token) Iteration.pageDecoder toMsg


decoderFromCred : Decoder (Token -> Int -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map3 (\fromCred cred -> fromCred cred)
        decoder
        tokenDecoder
        lastRefreshDecoder



type Error
    = BadStatus Int Error.Model
    | Timeout
    | BadUrl String
    | NetworkError
    | BadBody String


expectJson : (Result Error a -> msg) -> Decoder a -> Expect msg
expectJson toMsg decoder =
  Http.expectStringResponse toMsg <|
    \response ->
      case response of
        Http.BadUrl_ url ->
          Err (BadUrl url)

        Http.Timeout_ ->
          Err Timeout

        Http.NetworkError_ ->
          Err NetworkError

        Http.BadStatus_ metadata body ->
          Err (BadStatus metadata.statusCode (decodeErrors body))

        Http.GoodStatus_ metadata body ->
          case decodeString decoder body of
            Ok value ->
              Ok value

            Err err ->
              Err (BadBody (Decode.errorToString err))

{-| Many API endpoints include an "errors" field in their BadStatus responses.
-}
decodeErrors : String -> Error.Model
decodeErrors body =
    body
        |> decodeString Error.decoder
        |> Result.withDefault Error.unknownErrorResponse
