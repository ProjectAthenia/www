module Models.Payment.Payment exposing (..)

import Models.Payment.LineItem as LineItem
import Models.Payment.PaymentMethod as PaymentMethod
import Time exposing (Posix)


type alias Model =
    { id: Int
    , amount: Float
    , refunded_at: Maybe Posix
    , paymentMethod: Maybe PaymentMethod.Model
    , lineItems: List LineItem.Model
    }
