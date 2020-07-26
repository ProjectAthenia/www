module Models.MembershipPlan.Subscription exposing (..)

import Iso8601
import Json.Decode as JsonDecode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as JsonEncode
import Models.MembershipPlan.MembershipPlan as MembershipPlan
import Models.MembershipPlan.MembershipPlanRate as MembershipPlanRate
import Models.Page as Page
import Models.Payment.PaymentMethod as PaymentMethod
import Time exposing (..)


type alias Model =
    { id : Int
    , last_renewed_at : Posix
    , subscribed_at : Posix
    , expires_at : Maybe Posix
    , canceled_at : Maybe Posix
    , recurring: Bool
    , membership_plan_rate : Maybe MembershipPlanRate.Model
    , payment_method : Maybe PaymentMethod.Model
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


getActiveSubscriptions : Posix -> List Model -> List Model
getActiveSubscriptions now subscriptions =
    List.filter (isActive now) subscriptions


getCurrentSubscription : Posix -> List Model -> Maybe Model
getCurrentSubscription now subscriptions =
    List.head
        <| List.sortWith compareExpiration (getActiveSubscriptions now subscriptions)

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
        |> optional "membership_plan_rate" (JsonDecode.maybe MembershipPlanRate.modelDecoder) Nothing
        |> optional "payment_method" (JsonDecode.maybe PaymentMethod.modelDecoder) Nothing


listDecoder : JsonDecode.Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder


pageDecoder: JsonDecode.Decoder (Page.Model Model)
pageDecoder =
    Page.modelDecoder listDecoder


toCreateJson : CreateModel -> JsonEncode.Value
toCreateJson model =
    JsonEncode.object
        [ ("recurring", JsonEncode.bool model.recurring)
        , ("membership_plan_rate_id", JsonEncode.int model.membership_plan_rate_id)
        , ("payment_method_id", JsonEncode.int model.payment_method_id)
        ]


recurringJson : Bool -> JsonEncode.Value
recurringJson recurring =
    JsonEncode.object
        [ ("recurring", JsonEncode.bool recurring)
        ]


paymentMethodChangedJson : Int -> JsonEncode.Value
paymentMethodChangedJson paymentMethodId =
    JsonEncode.object
        [ ("payment_method_id", JsonEncode.int paymentMethodId)
        ]
