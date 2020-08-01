module Models.MembershipPlan.MembershipPlanRate exposing (..)

import Iso8601
import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Models.MembershipPlan.MembershipPlan as MembershipPlan
import Models.Page as Page
import Time exposing (Posix)
import Utilities.ModelHelpers exposing (..)


type alias Model =
    { id: Int
    , cost: Float
    , created_at : Maybe Posix
    , membership_plan: Maybe MembershipPlan.Model
    }


-- Decodes a user model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "cost" floatDecoder
        |> optional "created_at" (maybe Iso8601.decoder) Nothing
        |> optional "membership_plan" (maybe MembershipPlan.modelDecoder) Nothing


pageDecoder: Decoder (Page.Model Model)
pageDecoder =
    Page.modelDecoder <| list modelDecoder