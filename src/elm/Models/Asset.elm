-- Asset model for decoding background responses
module Models.Asset exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (required)
import Models.Page as Page


type alias Model =
    { id: Int
    , url: String
    }


type alias Page =
    Page.Model Model


model : Int -> String -> Model
model id url =
    { id = id
    , url = url
    }


-- Decodes a status model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "url" string


listDecoder : Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder


pageDecoder: Decoder Page
pageDecoder =
    Page.modelDecoder listDecoder
