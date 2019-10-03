module Models.Role exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as JsonEncode exposing (..)
import Models.Page as Page exposing (..)


appUser = 1
superAdmin = 2
articleViewer = 3
articleEditor = 4
-- Add additional roles below here if you would like


type alias Model =
    { id: Int
    , name: String
    }


cacheEncoder : Model -> JsonEncode.Value
cacheEncoder model =
    JsonEncode.object
        [ ( "id" , JsonEncode.int model.id)
        -- We dont need the name when we cache it, and it leaves it ambiguous if someone is digging around in the storage
        , ("name", JsonEncode.string "")
        ]


-- Decodes a user model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" JsonDecode.int
        |> required "name" JsonDecode.string


listDecoder : Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder


pageDecoder : Decoder (Page.Model Model)
pageDecoder =
    Page.modelDecoder listDecoder
