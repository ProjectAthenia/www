port module Ports.Stripe exposing (..)


type alias Model =
    { id : String
    , last4 : String
    }


-- Inits an actual stripe form
port initStripeForm : String -> Cmd msg


-- Creates a payment method
port createPaymentToken : String -> Cmd msg


-- called when a card has been created successfully
port tokenCreated : (String -> msg) -> Sub msg


port stripeError : (String -> msg) -> Sub msg
