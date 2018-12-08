-- Module for the user model
module Athenia.Models.User.User exposing(..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode


type alias Model =
    { id: Int
    , name: String
    , email: String
    , password : String
    , roles: List Int
    }


-- Converts a user model into a JSON string
toJson : Model -> Encode.Value
toJson model =
    Encode.object
        <| List.concat
            [ if String.length model.email > 0 then
                [ ("email", Encode.string model.email) ]
            else
                []
            , if String.length model.password > 0 then
                [ ("password", Encode.string model.password) ]
            else
                []
            , if String.length model.name > 0 then
                [ ("name", Encode.string model.name) ]
            else
                []
            ]


cacheEncoder : Model -> Encode.Value
cacheEncoder model =
    Encode.object
        [
            ( "id" , Encode.int model.id)
            , ("name", Encode.string model.name)
            , ("email", Encode.string model.email)
            , ("password", Encode.string model.password)
            , ("roles", (Encode.list Encode.int model.roles))
        ]


-- Decodes a user model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "name" string
        |> required "email" string
        |> hardcoded ""
        |> optional "roles" (list (at ["id"] int)) []


listDecoder : Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder
