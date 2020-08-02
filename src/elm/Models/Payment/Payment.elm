module Models.Payment.Payment exposing (..)

import Iso8601
import Json.Decode as JsonDecode exposing (Decoder, int, maybe)
import Json.Decode.Pipeline exposing (optional, required)
import Models.Payment.LineItem as LineItem
import Models.Payment.PaymentMethod as PaymentMethod
import Time exposing (Posix)
import Utilities.ModelHelpers exposing (floatDecoder)


type alias Model =
    { id: Int
    , amount: Float
    , refunded_at: Maybe Posix
    , payment_method: Maybe PaymentMethod.Model
    , line_items: List LineItem.Model
    }


modelDecoder: Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "amount" floatDecoder
        |> optional "refunded_at" (maybe Iso8601.decoder) Nothing
        |> optional "payment_method" (maybe PaymentMethod.modelDecoder) Nothing
        |> optional "line_items" LineItem.listDecoder []
