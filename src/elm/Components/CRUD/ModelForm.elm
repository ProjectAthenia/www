module Components.CRUD.ModelForm exposing (..)

import Api exposing (Error, Token)
import Api.Group exposing (..)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Components.LoadingIndicator as LoadingIndicator
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit)
import Http as Http exposing (..)
import Json.Decode exposing (..)
import Json.Encode as Encode
import Utilities.Expands as Expands


type Msg dataModel childMsg
    = ModelCreatedResponse (Result Api.Error dataModel)
    | ModelUpdatedResponse (Result Api.Error dataModel)
    | ModelLoadedResponse (Result Api.Error dataModel)
    | ChildMsg childMsg
    | Save


type alias ModelEncoder dataModel
    = dataModel -> Encode.Value


-- The init function format that must return our form model, and any needed commands
type alias ChildInit childModel childMsg =
    String -> Token -> (childModel, Cmd childMsg)


-- The update function format for any form update calls
type alias ChildUpdate dataModel childModel childMsg =
    Token -> dataModel -> childMsg -> childModel -> (childModel, Cmd childMsg)


-- An alias to our form view function that all of our forms must pass in
type alias ChildView dataModel childModel childMsg =
    dataModel -> childModel -> Html childMsg


type alias ChildSetDataModel dataModel childModel =
    childModel -> dataModel -> childModel

type alias Field childModel childMsg =
    childModel -> Html childMsg

type alias Configuration dataModel childModel childMsg =
    { createEncoder: ModelEncoder dataModel
    , updateEncoder: ModelEncoder dataModel
    , childInit: ChildInit childModel childMsg
    , childUpdate: ChildUpdate dataModel childModel childMsg
    , childView: ChildView dataModel childModel childMsg
    , setModel: ChildSetDataModel dataModel childModel
    , newModel: dataModel
    , title: String
    , fields: List (Field childModel childMsg)
    }

type alias Model dataModel childModel =
    { loading : Bool
    , decoder: Decoder dataModel
    , routeGroup: RouteGroup
    , dataModel : dataModel
    , childModel: childModel
    }


initialState : Configuration dataModel childModel childMsg -> RouteGroup -> List Expands.Expand -> Decoder dataModel -> String -> Token -> Maybe Int
    -> (Model dataModel childModel, Cmd (Msg dataModel childMsg))
initialState configuration routeGroup expandFields decoder apiUrl token maybeId =
    let
        (childModel, childCmd) =
            configuration.childInit apiUrl token
    in
    ( { loading = maybeId /= Nothing
      , decoder = decoder
      , routeGroup = routeGroup
      , dataModel = configuration.newModel
      , childModel = childModel
    }
    , Cmd.batch
        [ case maybeId of
            Just id ->
                getModel token routeGroup.existing expandFields id decoder
            Nothing ->
                Cmd.none
        , Cmd.map ChildMsg childCmd
        ]
    )


setModel : Configuration dataModel childModel childMsg -> Model dataModel childModel -> dataModel -> Bool -> Model dataModel childModel
setModel configuration model dataModel loading =
    { model
        | loading = loading
        , dataModel = dataModel
        , childModel = configuration.setModel model.childModel dataModel
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



createModel : Token -> NewEndpoint -> Decoder dataModel -> ModelEncoder dataModel -> dataModel -> Cmd (Msg dataModel childMsg)
createModel token newEndpoint decoder createEncoder model =
    Api.createModel newEndpoint token (Http.jsonBody (createEncoder model)) decoder ModelCreatedResponse


updateModel : Token -> ExistingEndpoint -> Int -> Decoder dataModel -> ModelEncoder dataModel -> dataModel -> Cmd (Msg dataModel childMsg)
updateModel token existingEndpoint id decoder updateEncoder model =
    Api.updateModel (existingEndpoint [] id) token (Http.jsonBody (updateEncoder model)) decoder ModelUpdatedResponse


getModel : Token -> ExistingEndpoint -> List Expands.Expand -> Int -> Decoder dataModel -> Cmd (Msg dataModel childMsg)
getModel token existingEndpoint expands id decoder =
    Api.get (existingEndpoint (Expands.toQueryParameters expands) id) (Just token) decoder ModelLoadedResponse