module Utilities.Configuration exposing (..)

import Dict exposing (..)
import Json.Decode as Decode exposing (Decoder, Value, list, dict, string)


type alias Model
    = Dict String String


decoder: Decode.Decoder Model
decoder =
    dict string


fetchConfigVariable: Model -> String -> Maybe String
fetchConfigVariable model key =
    Dict.get key model
