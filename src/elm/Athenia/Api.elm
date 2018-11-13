port module Athenia.Api exposing
    (Token
    , addServerError
    , application
    , decodeErrors
    , delete, get, post, put
    , login, logout, signUp
    , settings
    , storeCredWith
    , viewerChanges
    )

{-| This module is responsible for communicating to the Conduit API.

It exposes an opaque Endpoint type which is guaranteed to point to the correct URL.

-}

import Athenia.Api.Endpoint as Endpoint
import Athenia.Models.User.User as User
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
credDecoder : Decoder Token
credDecoder =
    Decode.succeed Token
        |> required "token" Decode.string



-- PERSISTENCE


decode : Decoder (Token -> viewer) -> Value -> Result Decode.Error viewer
decode decoder value =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue Decode.string value
        |> Result.andThen (\str -> Decode.decodeString (Decode.field "user" (decoderFromCred decoder)) str)


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Maybe viewer -> msg) -> Decoder (Token -> viewer) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder (Token -> viewer) -> Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue (storageDecoder viewerDecoder) val
        |> Result.toMaybe


storeCredWith : Token -> User.Model -> Cmd msg
storeCredWith (Token token) user =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "token", Encode.string token )
                        , ( "model", User.toJson user )
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
    Decoder (Token -> viewer)
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


storageDecoder : Decoder (Token -> viewer) -> Decoder viewer
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


login : Http.Body -> Decoder (Token -> User.Model) -> Http.Request User.Model
login body decoder =
    post Endpoint.login Nothing body User.modelDecoder


signUp : Http.Body -> Decoder (Token -> User.Model) -> Http.Request User.Model
signUp body decoder =
    post Endpoint.signUp Nothing body User.modelDecoder


settings : Token -> Int -> Http.Body -> Http.Request User.Model
settings token userId body =
    put (Endpoint.user userId) token body User.modelDecoder


me : Token -> Http.Request User.Model
me token =
    get Endpoint.me (Just token) User.modelDecoder


decoderFromCred : Decoder (Token -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder



-- ERRORS


addServerError : List String -> List String
addServerError list =
    "Server error" :: list


{-| Many API endpoints include an "errors" field in their BadStatus responses.
-}
decodeErrors : Http.Error -> List String
decodeErrors error =
    case error of
        Http.BadStatus response ->
            response.body
                |> decodeString (field "errors" errorsDecoder)
                |> Result.withDefault [ "Server error" ]

        err ->
            [ "Server error" ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors



-- LOCALSTORAGE KEYS


cacheStorageKey : String
cacheStorageKey =
    "cache"


credStorageKey : String
credStorageKey =
    "cred"