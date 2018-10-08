-- Port for dealing with local storage
port module Ports.LocalStorage exposing (..)


type alias Model =
    { auth_token : Maybe String
    , retrieved_at : Maybe String
    }


port initLocalStorage : () -> Cmd msg


port localStorageReady : (Model -> msg) -> Sub msg


port setLocalStorageValue : (String, String) -> Cmd msg


port clearStorage : () -> Cmd msg
