module Athenia.Viewer exposing (Viewer, avatar, cred, decoder, minPasswordChars, store, username)

{-| The logged-in user currently viewing this page. It stores enough data to
be able to render the menu bar (username and avatar), along with Cred so it's
impossible to have a Viewer if you aren't logged in.
-}

import Athenia.Api as Api exposing (Token)
import Athenia.Avatar exposing (Avatar)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)


-- TYPES


type Viewer
    = Viewer Avatar Token


-- INFO


avatar : Viewer -> Avatar
avatar (Viewer val _) =
    val


cred : Viewer -> Token
cred (Viewer _ val) =
    val


{-| Passwords must be at least this many characters long!
-}
minPasswordChars : Int
minPasswordChars =
    6



-- SERIALIZATION


store : Viewer -> Cmd msg
store (Viewer avatarVal tokenVal) =
    Api.storeCredWith
        tokenVal
        avatarVal
