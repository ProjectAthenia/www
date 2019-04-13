module Models.MembershipPlan.Subscription exposing (..)

import Iso8601
import Json.Decode as JsonDecode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as JsonEncode
import Models.MembershipPlan.MembershipPlan as MembershipPlan
import Models.MembershipPlan.MembershipPlanRate as MembershipPlanRate
import Models.Payment.PaymentMethod as PaymentMethod
import Time exposing (..)


type alias Model =
    { id : Int
    , last_renewed_at : Posix
    , subscribed_at : Posix
    , expires_at : Maybe Posix
    , canceled_at : Maybe Posix
    , recurring: Bool
    , membership_plan : MembershipPlan.Model
    , membership_plan_rate : MembershipPlanRate.Model
    , payment_method : PaymentMethod.Model
    }


type alias CreateModel =
    { recurring: Bool
    , membership_plan_rate_id : Int
    , payment_method_id : Int
    }


compareExpiration : Model -> Model -> Order
compareExpiration subscriptionA subscriptionB =
    case (subscriptionA.expires_at, subscriptionB.expires_at) of
        (Nothing, _) ->
            LT
        (_, Nothing) ->
            GT
        (Just expiresA, Just expiresB) ->
            if (posixToMillis expiresA < posixToMillis expiresB) then
                LT
            else
                GT



isActive : Posix -> Model -> Bool
isActive now model =
    case model.expires_at of
        Nothing ->
            True
        Just expiresAt ->
            (posixToMillis expiresAt) > (posixToMillis now)


createModel : Bool -> MembershipPlan.Model -> PaymentMethod.Model -> CreateModel
createModel recurring membershipPlan paymentMethod =
    { recurring = recurring
    , membership_plan_rate_id = membershipPlan.current_rate_id
    , payment_method_id = paymentMethod.id
    }


modelDecoder : JsonDecode.Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" JsonDecode.int
        |> required "last_renewed_at" Iso8601.decoder
        |> required "subscribed_at" Iso8601.decoder
        |> required "expires_at" (JsonDecode.maybe Iso8601.decoder)
        |> required "canceled_at" (JsonDecode.maybe Iso8601.decoder)
        |> required "recurring" JsonDecode.bool
        |> required "membership_plan" MembershipPlan.modelDecoder
        |> required "membership_plan_rate" MembershipPlanRate.modelDecoder
        |> required "payment_method" PaymentMethod.modelDecoder


listDecoder : JsonDecode.Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder


toCreateJson : CreateModel -> JsonEncode.Value
toCreateJson model =
    JsonEncode.object
        [ ("recurring", JsonEncode.bool model.recurring)
        , ("membership_plan_rate_id", JsonEncode.int model.membership_plan_rate_id)
        , ("payment_method_id", JsonEncode.int model.payment_method_id)
        ]


toUpdateJson : Model -> JsonEncode.Value
toUpdateJson model =
    JsonEncode.object
        [ ("recurring", JsonEncode.bool model.recurring)
        ]


cancelJson : JsonEncode.Value
cancelJson =
    JsonEncode.object
        [ ("cancel", JsonEncode.bool True)
        ]
