port module Ports.Stripe exposing (..)


type alias Model =
    { id : String
    , last4 : String
    }


type alias CreateModel =
    { number : String
    , exp_month : String
    , exp_year : String
    , cvc : String
    }


-- Creates a payment method
port createPaymentMethod : (String, CreateModel) -> Cmd msg


-- Loads a payment method off of the stripe server
-- the first param is the customer id, and the second is the card id
port retrievePaymentMethod : (String, String) -> Cmd msg


-- called when an existing card has been loaded from stripe
port cardRetrieved : (Model -> msg) -> Sub msg


-- called when a card has been created successfully
port cardCreated : (Model -> msg) -> Sub msg


port error : (String -> msg) -> Sub msg
