-- Utility model for errors returned from the API
module Athenia.Models.Error exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (required, optional)


type alias Model =
    { message : String
    , errors : (List (String, List String))
    }


-- Decodes an error retrieved through the API
decoder : Decoder Model
decoder =
    JsonDecode.succeed Model
        |> required "message" string
        |> optional "errors" (keyValuePairs (list string)) []