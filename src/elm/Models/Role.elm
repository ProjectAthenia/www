module Models.Role exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)


appUser = 1
superAdmin = 2
articleViewer = 3
articleEditor = 4
-- Add additional roles below here if you would like


type alias Model =
    { id: Int
    , name: String
    }


-- Decodes a user model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "name" string


listDecoder : Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder