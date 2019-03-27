module Session exposing
    ( Session
    , changes
    , token, lastRefresh, user
    , needsAuthRefresh
    , fromViewer, navKey, viewer
    )

import Api as Api
import Models.User.User as User
import Utilities.AuthManager as AuthManager
import Viewer as Viewer
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Time


-- TYPES


type Session
    = LoggedIn Nav.Key Viewer.Viewer
    | Guest Nav.Key



-- INFO


viewer : Session -> Maybe Viewer.Viewer
viewer session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            Nothing


token : Session -> Maybe Api.Token
token session =
    case session of
        LoggedIn _ val ->
            Just (Viewer.token val)

        Guest _ ->
            Nothing


lastRefresh : Session -> Maybe Int
lastRefresh session =
    case session of
        LoggedIn _ val ->
            Just (Viewer.lastRefresh val)

        Guest _ ->
            Nothing


user : Session -> Maybe User.Model
user session =
    case session of
        LoggedIn _ val ->
            Just (Viewer.user val)

        Guest _ ->
            Nothing


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key


needsAuthRefresh : Time.Posix -> Session -> Bool
needsAuthRefresh currentTime session =
    case viewer session of
        Just sessionViewer ->
            AuthManager.needsRefresh currentTime (Viewer.lastRefresh sessionViewer)
        Nothing ->
            False


-- CHANGES


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.viewerChanges (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Viewer.decoder


fromViewer : Nav.Key -> Maybe Viewer.Viewer -> Session
fromViewer key maybeViewer =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    case maybeViewer of
        Just viewerVal ->
            LoggedIn key viewerVal

        Nothing ->
            Guest key