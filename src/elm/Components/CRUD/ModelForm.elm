module Components.CRUD.ModelForm exposing (..)

import Api exposing (Error, Token)
import Api.Group exposing (..)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Components.LoadingIndicator as LoadingIndicator
import Components.Toast as Toast
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit)
import Http as Http exposing (..)
import Json.Decode exposing (..)
import Json.Encode as Encode
import List.Extra as ListExtra
import Utilities.Expands as Expands
import Utilities.ModelHelpers exposing (..)


type Msg dataModel childMsg
    = ModelCreatedResponse (Result Api.Error (GenericModel dataModel))
    | ModelUpdatedResponse (Result Api.Error (GenericModel dataModel))
    | ModelLoadedResponse (Result Api.Error (GenericModel dataModel))
    | ChildMsg childMsg
    | RemoveToast Toast.Model
    | Save


type alias ModelEncoder dataModel
    = GenericModel dataModel -> Encode.Value


type alias ValidateModel dataModel
    = GenericModel dataModel -> Result String (GenericModel dataModel)


-- The init function format that must return our form model, and any needed commands
type alias ChildInit childModel childMsg =
    String -> Token -> (childModel, Cmd childMsg)


-- The update function format for any form update calls
type alias ChildUpdate dataModel childModel childMsg =
    Token -> (GenericModel dataModel) -> childMsg -> childModel -> (childModel, Cmd childMsg)


-- An alias to our form view function that all of our forms must pass in
type alias ChildView dataModel childModel childMsg =
    GenericModel dataModel -> childModel -> Html childMsg


type alias ChildAction dataModel childModel childMsg =
    Token -> (GenericModel dataModel) -> childModel -> (childModel, Cmd childMsg)


type alias Field childModel childMsg =
    childModel -> Html childMsg


type alias Configuration dataModel childModel childMsg =
    { createEncoder: ModelEncoder dataModel
    , updateEncoder: ModelEncoder dataModel
    , decoder: Decoder (GenericModel dataModel)
    , routeGroup: RouteGroup
    , childInit: ChildInit childModel childMsg
    , childUpdate: ChildUpdate dataModel childModel childMsg
    , childView: ChildView dataModel childModel childMsg
    , setModel: ChildAction dataModel childModel childMsg
    , modelCreated: ChildAction dataModel childModel childMsg
    , modelUpdated: ChildAction dataModel childModel childMsg
    , newModel: GenericModel dataModel
    , validateModel: ValidateModel dataModel
    , title: String
    , fields: List (Field childModel childMsg)
    }


type alias Model dataModel childModel =
    { loading : Bool
    , dataModel : (GenericModel dataModel)
    , childModel: childModel
    , toasts: List Toast.Model
    }


initialState : Configuration dataModel childModel childMsg -> List Expands.Expand -> String -> Token -> Maybe Int
    -> (Model dataModel childModel, Cmd (Msg dataModel childMsg))
initialState configuration expandFields apiUrl token maybeId =
    let
        (childModel, childCmd) =
            configuration.childInit apiUrl token
    in
    ( { loading = maybeId /= Nothing
      , dataModel = configuration.newModel
      , childModel = childModel
      , toasts = []
    }
    , Cmd.batch
        [ case maybeId of
            Just id ->
                getModel token configuration.routeGroup.existing expandFields id configuration.decoder
            Nothing ->
                Cmd.none
        , Cmd.map ChildMsg childCmd
        ]
    )


setModel : Token -> Configuration dataModel childModel childMsg -> Model dataModel childModel -> (GenericModel dataModel) -> Bool -> (Model dataModel childModel, Cmd (Msg dataModel childMsg))
setModel token configuration model dataModel loading =
    let
        (childModel, childMsg) =
            configuration.setModel token dataModel model.childModel
    in
    ( { model
        | loading = loading
        , dataModel = dataModel
        , childModel = childModel
    }
    , Cmd.map ChildMsg childMsg
    )


update : Token -> Configuration dataModel childModel childMsg -> Msg dataModel childMsg -> Model dataModel childModel -> (Model dataModel childModel, Cmd (Msg dataModel childMsg))
update token configuration msg model =
    case msg of
        ModelCreatedResponse (Ok dataModel) ->
            case dataModel.id of
                Just _ ->
                    let
                        (updatedModel, toastCmd) =
                            Toast.appendToast
                                (Toast.createToast Toast.Success RemoveToast "Created Successfully")
                                model

                        (childModel, childMsg) =
                            configuration.modelCreated token dataModel model.childModel
                    in
                    ( { updatedModel
                        | childModel = childModel
                    }
                    , Cmd.batch
                        [ toastCmd
                        , Cmd.map ChildMsg childMsg
                        ]
                    )

                Nothing ->
                    Toast.appendToast
                        (Toast.createToast Toast.Error RemoveToast "Unknown error creating")
                        { model | loading = False }

        ModelCreatedResponse (Err error) ->
            Toast.appendToast
                (Toast.createToast Toast.Error RemoveToast <| Api.createErrorText "Error Creating Radical" error)
                { model | loading = False }

        ModelUpdatedResponse (Ok dataModel) ->
            let
                (updatedModel, toastCmd) =
                    Toast.appendToast
                        (Toast.createToast Toast.Success RemoveToast "Created Successfully")
                        model

                (childModel, childMsg) =
                    configuration.modelUpdated token dataModel model.childModel
            in
            ( { updatedModel
                | childModel = childModel
                , loading = False
            }
            , Cmd.batch
                [ toastCmd
                , Cmd.map ChildMsg childMsg
                ]
            )

        ModelUpdatedResponse (Err error) ->
            Toast.appendToast
                (Toast.createToast Toast.Error RemoveToast <| Api.createErrorText "Error Updating Radical" error)
                { model | loading = False }

        ModelLoadedResponse (Ok dataModel) ->
            setModel token configuration model dataModel False

        ModelLoadedResponse (Err error) ->
            Toast.appendToast
                (Toast.createToast Toast.Error RemoveToast <| Api.createErrorText "Error Loading Radical" error)
                { model | loading = False }

        ChildMsg childMsg ->
            Tuple.mapBoth (setChildModel model) (Cmd.map ChildMsg)
                <| configuration.childUpdate token model.dataModel childMsg model.childModel

        RemoveToast toast ->
            ( { model
                | toasts = ListExtra.remove toast model.toasts
            }
            , Cmd.none
            )

        Save ->
            case configuration.validateModel model.dataModel of
                Err errorMessage ->
                    Toast.appendToast
                        (Toast.createToast Toast.Error RemoveToast errorMessage)
                        model

                Ok dataModel ->
                    ( { model
                        | loading = True
                    }
                    , case dataModel.id of
                        Just id ->
                            updateModel token configuration.routeGroup.existing id configuration.decoder configuration.updateEncoder dataModel
                        Nothing ->
                            createModel token configuration.routeGroup.new configuration.decoder configuration.createEncoder dataModel
                    )


noAction: ChildAction dataModel childModel childMsg
noAction token model childModel =
    ( childModel, Cmd.none )


setChildModel: Model dataModel childModel -> childModel -> Model dataModel childModel
setChildModel model childModel =
    { model
        | childModel = childModel
    }


view : Configuration dataModel childModel childMsg -> Model dataModel childModel -> List (Html msg) -> Html (Msg dataModel childMsg)
view configuration model fields =
    div [ class "model_form" ]
        [ h1 [] [ text configuration.title ]
        , Form.form
            [ onSubmit Save ]
                <| List.concat
                    [ (List.map (\field -> Html.map ChildMsg <| field model.childModel) configuration.fields)
                    , [submitButton model.loading]
                    ]
        , LoadingIndicator.view model.loading
        ]


submitButton : Bool -> Html msg
submitButton loading =
    Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ disabled loading ]
        ]
        [ text "Save" ]


createModel : Token -> NewEndpoint -> Decoder (GenericModel dataModel) -> ModelEncoder dataModel -> GenericModel dataModel -> Cmd (Msg dataModel childMsg)
createModel token newEndpoint decoder createEncoder model =
    Api.createModel newEndpoint token (Http.jsonBody (createEncoder model)) decoder ModelCreatedResponse


updateModel : Token -> ExistingEndpoint -> Int -> Decoder (GenericModel dataModel) -> ModelEncoder dataModel -> GenericModel dataModel -> Cmd (Msg dataModel childMsg)
updateModel token existingEndpoint id decoder updateEncoder model =
    Api.updateModel (existingEndpoint [] id) token (Http.jsonBody (updateEncoder model)) decoder ModelUpdatedResponse


getModel : Token -> ExistingEndpoint -> List Expands.Expand -> Int -> Decoder (GenericModel dataModel) -> Cmd (Msg dataModel childMsg)
getModel token existingEndpoint expands id decoder =
    Api.get (existingEndpoint (Expands.toQueryParameters expands) id) (Just token) decoder ModelLoadedResponse