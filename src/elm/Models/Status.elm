-- Status Model returned from the API
module Models.Status exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (required)


type alias Model =
    { status: String
    }


-- Decodes a status model retrieved through the API
decoder : Decoder Model
decoder =
    JsonDecode.succeed Model
        |> required "status" string
