module Models.MembershipPlan.MembershipPlanRate exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Models.MembershipPlan.MembershipPlan as MembershipPlan
import Utilities.ModelHelpers exposing (..)


type alias Model =
    { id: Int
    , cost: Float
    , membership_plan: MembershipPlan.Model
    }


-- Decodes a user model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "cost" stringFloatDecoder
        |> required "membership_plan" MembershipPlan.modelDecoder
