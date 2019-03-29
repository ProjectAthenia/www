module Models.MembershipPlan.MembershipPlanRate exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias Model =
    { id: Int
    , cost: Float
    }


-- Decodes a user model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "cost" float
