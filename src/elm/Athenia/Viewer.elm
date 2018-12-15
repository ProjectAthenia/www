module Athenia.Viewer exposing (Viewer, viewer, user, decoder, token, maybeToken, minPasswordChars, store)

{-| The logged-in user currently viewing this page. It stores enough data to
be able to render the menu bar (username and avatar), along with Cred so it's
impossible to have a Viewer if you aren't logged in.
-}

import Athenia.Api as Api exposing (Token)
import Athenia.Avatar exposing (Avatar)
import Athenia.Models.User.User as User
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)


-- TYPES


type Viewer
    = Viewer User.Model Token


-- INFO


user : Viewer -> User.Model
user (Viewer val _) =
    val


maybeToken : Maybe Viewer -> Maybe Token
maybeToken maybeViewer =
    case maybeViewer of
        Just viewerInstance ->
            Just (token viewerInstance)
        Nothing ->
            Nothing

token : Viewer -> Token
token (Viewer _ val) =
    val


{-| Passwords must be at least this many characters long!
-}
minPasswordChars : Int
minPasswordChars =
    6


viewer : User.Model -> Token -> Viewer
viewer userVal tokenVal =
    Viewer userVal tokenVal


-- SERIALIZATION

decoder : Decoder (Token -> Viewer)
decoder =
    Decode.succeed Viewer
        |> custom (Decode.field "model" User.modelDecoder)


store : Viewer -> Cmd msg
store (Viewer userVal tokenVal) =
    Api.storeCredWith
        tokenVal
        userVal
