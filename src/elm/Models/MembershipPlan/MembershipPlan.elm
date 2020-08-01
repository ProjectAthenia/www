module Models.MembershipPlan.MembershipPlan exposing (..)

import Api.Group exposing (RouteGroup, baseGroup)
import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Models.Page as Page
import Utilities.ModelHelpers exposing (..)

type alias Record
    = { name: String , duration: String , current_cost: Float , current_rate_id: Int}

type alias Model =
    GenericModel Record

type alias Page
    = Page.Model Model


newModel: Model
newModel =
    { id = Nothing
    , name = ""
    , duration = ""
    , current_cost = 0.00
    , current_rate_id = 0
    }


makeReadable : Model -> String
makeReadable model =
    model.name ++ " ($" ++ String.fromFloat model.current_cost ++ ")"


recordDecoder: Decoder Record
recordDecoder =
    JsonDecode.succeed Record
        |> required "name" string
        |> required "duration" string
        |> required "current_cost" floatDecoder
        |> required "current_rate_id" int


mergeModels: BaseRecord -> Record -> Model
mergeModels baseRecord record =
    { id = baseRecord.id
    , name = record.name
    , duration = record.duration
    , current_cost = record.current_cost
    , current_rate_id = record.current_rate_id
    }


modelDecoder: Decoder (GenericModel Model)
modelDecoder =
    map2 mergeModels baseRecordDecoder recordDecoder


listDecoder : Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder


pageDecoder : Decoder Page
pageDecoder =
    Page.modelDecoder listDecoder


-- Converts a user model into a JSON string
toJson : GenericModel Model -> Encode.Value
toJson model =
    Encode.object
        [ ("name", Encode.string model.name)
        , ("duration", Encode.string model.duration)
        , ("current_cost", Encode.float model.current_cost)
        ]


routeGroup: String -> RouteGroup
routeGroup =
    baseGroup "membership-plans"
