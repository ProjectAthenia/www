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
import Models.User.User as User
import Utilities.SearchField exposing (SearchFieldType(..))
import Utilities.ModelHelpers exposing (..)


type alias Model =
    RootController.Model User.Model FormModel FormMsg


type alias Msg =
    RootController.Msg User.Model FormMsg


type alias FormModel =
    { name: String
    , duration: String
    , current_cost: Float
    }

type FormMsg
    = SetName String
    | SetDuration String
    | SetCurrentCost String


sharedConfiguration: String -> SharedConfiguration.Configuration User.Model
sharedConfiguration apiUrl =
    SharedConfiguration.configure "Membership Plan" "membership-plans" (User.routeGroup apiUrl) User.modelDecoder []


nameColumn: ModelList.Column User.Model
nameColumn =
     ModelList.column
        "Name"
        (\membershipPlan -> Html.text membershipPlan.name)
        "name"
        Text


durationColumn: ModelList.Column User.Model
durationColumn =
     ModelList.column
        "Duration"
        (\membershipPlan -> Html.text membershipPlan.duration)
        "duration"
        (Select [("lifetime", "Lifetime"), ("monthly", "Monthly"), ("yearly", "Yearly")])


currentCostColumn: ModelList.Column User.Model
currentCostColumn =
     ModelList.column
        "Current Cost"
        (\membershipPlan -> Html.text (String.fromFloat membershipPlan.current_cost))
        ""
        None


indexColumns: List (ModelList.Column User.Model)
indexColumns =
    [ nameColumn
    , durationColumn
    , currentCostColumn
    ]


indexConfiguration: ModelList.Configuration User.Model
indexConfiguration =
    ModelList.configure [] indexColumns


validateForm: User.Model -> FormModel -> Result String User.Model
validateForm model form =
    if String.length form.name < 1 then
        Err "Please enter the membership plan name"
    else if String.length form.duration < 1 then
        Err "Please select a duration"
    else
        Ok { model
            | name = form.name
            , duration = form.duration
            , current_cost = form.current_cost
           }


initForm: Token -> (FormModel, Cmd FormMsg)
initForm token =
    ( { name = ""
      , duration = ""
      , current_cost = 0.0
    }
    , Cmd.none
    )


updateForm: Token -> User.Model -> FormMsg -> FormModel -> (FormModel, Cmd FormMsg)
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


setModel: Token -> GenericModel User.Model -> FormModel -> (FormModel, Cmd FormMsg)
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


formConfiguration: ModelForm.Configuration User.Model FormModel FormMsg
formConfiguration =
    let
        baseConfig = ModelForm.configure User.toJson User.toJson User.newModel validateForm initForm updateForm
    in
    ModelForm.addFields (ModelForm.configureSetModelAction setModel baseConfig)
        [ nameInput
        , durationSelect
        , currentCostInput
        ]


configuration: String -> RootController.Configuration User.Model FormModel FormMsg
configuration apiUrl =
    RootController.configure (sharedConfiguration apiUrl) indexConfiguration formConfiguration


initialModel: String -> RootController.Model User.Model FormModel FormMsg
initialModel apiUrl =
    RootController.initialState
        <| configuration apiUrl
