module Models.MembershipPlan.MembershipPlan exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Models.Page as Page


type alias Model =
    { id: Int
    , name: String
    , duration: String
    , current_cost: Float
    , current_rate_id: Int
    }

type alias Page
    = Page.Model Model

makeReadable : Model -> String
makeReadable model =
    model.name ++ " ($" ++ String.fromFloat model.current_cost ++ ")"


-- Decodes a user model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "name" string
        |> required "duration" string
        |> required "current_cost" stringFloatDecoder
        |> required "current_rate_id" int

stringFloatDecoder : Decoder Float
stringFloatDecoder =
  (JsonDecode.string)
      |> JsonDecode.andThen (\val ->
          case String.toFloat val of
              Just f -> JsonDecode.succeed f
              _ -> JsonDecode.fail "Error transforming string encoded float to float"
          )

listDecoder : Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder


pageDecoder : Decoder Page
pageDecoder =
    Page.modelDecoder listDecoder
