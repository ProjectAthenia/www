module Models.MembershipPlan.MembershipPlan exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias Model =
    { id: Int
    , name: String
    , duration: String
    , current_cost: Float
    , current_rate_id: Int
    }


makeReadable : Model -> String
makeReadable model =
    model.name ++ " (" ++ String.fromFloat model.current_cost ++ ")"


-- Decodes a user model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "name" string
        |> required "duration" string
        |> required "current_cost" float
        |> required "current_rate_id" int
