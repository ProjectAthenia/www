module Athenia.Models.Wiki.Iteration exposing (..)

import Athenia.Models.User.User as User
import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias Model =
    { id : Int
    , content : String
    , created_by : Maybe User.Model
    }


-- Decodes a article model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "content" string
        |> optional "created_by" (maybe User.modelDecoder) Nothing


listDecoder : Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder
