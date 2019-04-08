module Models.Payment.PaymentMethod exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode


type alias Model =
    { id: Int
    , payment_method_key: String
    , payment_method_type: String
    , identifier : Maybe String
    }


type alias CreateModel =
    { payment_method_key: String
    , payment_method_type: String
    }


-- Converts a payment method model into a JSON string for creation on the server
toCreateJson: CreateModel -> Encode.Value
toCreateJson model =
    Encode.object
        [ ("payment_method_key", Encode.string model.payment_method_key)
        , ("payment_method_type", Encode.string model.payment_method_type)
        ]


-- Decodes a payment method model retrieved through the API
modelDecoder: Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "payment_method_key" string
        |> required "payment_method_type" string
        |> optional "identifier" (maybe string) Nothing


makeReadable : Model -> String
makeReadable model =
    case model.identifier of
        Just identifier ->
            "Credit Card Ending in" ++ identifier
        Nothing ->
            "Existing Credit Card"


listDecoder: Decoder (List Model)
listDecoder =
    list modelDecoder
