module Page.Admin.Sections.MembershipPlan exposing (..)

import Api exposing (Token)
import Components.CRUD.ModelForm.Input as Input
import Components.CRUD.ModelForm.NumberField as NumberField
import Components.CRUD.ModelForm.TextField as TextField
import Components.CRUD.ModelForm.SelectField as SelectField
import Components.CRUD.ModelForm as ModelForm
import Components.CRUD.ModelList as ModelList
import Components.CRUD.RootController as RootController
import Components.CRUD.SharedConfiguration as SharedConfiguration
import Html exposing (..)
import Models.MembershipPlan.MembershipPlan as MembershipPlan
import Utilities.SearchField exposing (SearchFieldType(..))
import Utilities.ModelHelpers exposing (..)


type alias Model =
    RootController.Model MembershipPlan.Model FormModel FormMsg


type alias Msg =
    RootController.Msg MembershipPlan.Model FormMsg


type alias FormModel =
    { name: String
    , duration: String
    , current_cost: Float
    }

type FormMsg
    = SetName String
    | SetDuration String
    | SetCurrentCost String


sharedConfiguration: String -> SharedConfiguration.Configuration MembershipPlan.Model
sharedConfiguration apiUrl =
    SharedConfiguration.configure "Membership Plan" "membership-plans" (MembershipPlan.routeGroup apiUrl) MembershipPlan.modelDecoder []


nameColumn: ModelList.Column MembershipPlan.Model
nameColumn =
     ModelList.column
        "Name"
        (\membershipPlan -> Html.text membershipPlan.name)
        "name"
        Text


durationColumn: ModelList.Column MembershipPlan.Model
durationColumn =
     ModelList.column
        "Duration"
        (\membershipPlan -> Html.text membershipPlan.duration)
        "duration"
        (Select [("lifetime", "Lifetime"), ("monthly", "Monthly"), ("yearly", "Yearly")])


currentCostColumn: ModelList.Column MembershipPlan.Model
currentCostColumn =
     ModelList.column
        "Current Cost"
        (\membershipPlan -> Html.text (String.fromFloat membershipPlan.current_cost))
        ""
        None


indexColumns: List (ModelList.Column MembershipPlan.Model)
indexColumns =
    [ nameColumn
    , durationColumn
    , currentCostColumn
    ]


indexConfiguration: ModelList.Configuration MembershipPlan.Model
indexConfiguration =
    ModelList.configure [] indexColumns


validateForm: MembershipPlan.Model -> FormModel -> Result String MembershipPlan.Model
validateForm model form =
    Ok model


initForm: Token -> (FormModel, Cmd FormMsg)
initForm token =
    ( { name = ""
      , duration = ""
      , current_cost = 0.0
    }
    , Cmd.none
    )


updateForm: Token -> MembershipPlan.Model -> FormMsg -> FormModel -> (FormModel, Cmd FormMsg)
updateForm token dataModel msg model =
    case msg of
        SetName name ->
            ( { model
                | name = name
            }
            , Cmd.none
            )

        SetDuration duration ->
            ( { model
                | duration = duration
            }
            , Cmd.none
            )

        SetCurrentCost currentCost ->
            ( case String.toFloat currentCost of
                Just cost ->
                    { model
                        | current_cost = cost
                    }

                Nothing ->
                    model
            , Cmd.none
            )


setModel: Token -> GenericModel MembershipPlan.Model -> FormModel -> (FormModel, Cmd FormMsg)
setModel token dataModel model =
    ( { model
      | name = dataModel.name
      , duration = dataModel.duration
      , current_cost = dataModel.current_cost
    }
    , Cmd.none
    )


nameInput: Bool -> FormModel -> Html FormMsg
nameInput isLoading model =
    TextField.view (Input.configure True "Name" "name") model.name SetName isLoading


durationOptions: List (String, String)
durationOptions =
    [ ("lifetime", "Lifetime")
    , ("yearly", "Yearly")
    , ("monthly", "Monthly")
    ]


durationSelect: Bool -> FormModel -> Html FormMsg
durationSelect isLoading model =
    SelectField.view (SelectField.configure durationOptions "duration" "Duration" True) model.duration SetDuration isLoading


currentCostInput: Bool -> FormModel -> Html FormMsg
currentCostInput isLoading model =
    NumberField.view (Input.configure True "Current Cost" "current_cost") model.current_cost SetCurrentCost isLoading


formConfiguration: ModelForm.Configuration MembershipPlan.Model FormModel FormMsg
formConfiguration =
    let
        baseConfig = ModelForm.configure MembershipPlan.toJson MembershipPlan.toJson MembershipPlan.newModel validateForm initForm updateForm
    in
    ModelForm.addFields (ModelForm.configureSetModelAction setModel baseConfig)
        [ nameInput
        , durationSelect
        , currentCostInput
        ]


configuration: String -> RootController.Configuration MembershipPlan.Model FormModel FormMsg
configuration apiUrl =
    RootController.configure (sharedConfiguration apiUrl) indexConfiguration formConfiguration


initialModel: String -> RootController.Model MembershipPlan.Model FormModel FormMsg
initialModel apiUrl =
    RootController.initialState
        <| configuration apiUrl
