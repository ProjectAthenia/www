-- Module for the user model
module Models.User.User exposing(..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Models.Role as Role
import Models.Payment.PaymentMethod as PaymentMethod


type alias Model =
    { id: Int
    , name: String
    , email: String
    , password: String
    , payment_methods: List PaymentMethod.Model
    , roles: List Role.Model
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
        [ ( "id" , Encode.int model.id)
        , ("name", Encode.string model.name)
        , ("email", Encode.string model.email)
        ]


-- Decodes a user model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "name" string
        |> required "email" string
        |> hardcoded ""
        |> optional "payment_methods" PaymentMethod.listDecoder []
        |> optional "roles" Role.listDecoder []


listDecoder : Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder
