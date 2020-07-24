module Models.MembershipPlan.MembershipPlanRate exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Models.MembershipPlan.MembershipPlan as MembershipPlan
import Models.Page as Page
import Utilities.ModelHelpers exposing (..)


type alias Model =
    { id: Int
    , cost: Float
    , membership_plan: Maybe MembershipPlan.Model
    }


-- Decodes a user model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "cost" stringFloatDecoder
        |> optional "membership_plan" (maybe MembershipPlan.modelDecoder) Nothing


pageDecoder: Decoder (Page.Model Model)
pageDecoder =
    Page.modelDecoder <| list modelDecoder