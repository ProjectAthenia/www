module Models.Payment.LineItem exposing (..)

import Json.Decode as JsonDecode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import Utilities.ModelHelpers exposing (floatDecoder)


type alias Model =
    { id: Int
    , item_id: Int
    , item_type: String
    , amount: Float
    }


modelDecoder: Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "item_id" int
        |> required "item_type" string
        |> required "amount" floatDecoder
