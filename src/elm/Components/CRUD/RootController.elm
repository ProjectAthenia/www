module Components.CRUD.RootController exposing (..)

import Api exposing (Token)
import Api.Group exposing (..)
import Browser.Navigation as Navigation
import Components.CRUD.ModelForm as ModelForm
import Components.CRUD.ModelList as ModelList
import Components.Toast as Toast
import Html exposing (..)
import Json.Decode exposing (..)
import Url.Parser as Parser exposing ((</>), Parser, int)
import Utilities.Expands as Expands
import Utilities.ModelHelpers exposing (GenericModel)


type Route
    = Create
    | Index
    | Update Int


type State dataModel formModel
    = Inactive
    | IndexActive (ModelList.Model dataModel)
    | FormActive (ModelForm.Model dataModel formModel)


type Msg dataModel formMsg
    = FormMsg (ModelForm.Msg dataModel formMsg)
    | IndexMsg (ModelList.Msg dataModel)


-- The init function format that must return our form model, and any needed commands
type alias FormInit formModel formMsg =
    Token -> Maybe Int -> (formModel, Cmd formMsg)


-- The update function format for any form update calls
type alias FormUpdate formMsg formModel =
    Token -> formMsg -> formModel -> (formModel, Cmd formMsg)


-- An alias to our form view function that all of our forms must pass in
type alias FormView formModel formMsg =
    String -> formModel -> Html formMsg


type alias Configuration dataModel formModel formMsg =
    { resourceName: String
    , apiUrl : String
    , routeGroup: RouteGroup
    , decoder: Decoder (GenericModel dataModel)
    , expands : List Expands.Expand
    , indexConfiguration: ModelList.Configuration dataModel
    , formConfiguration: ModelForm.Configuration dataModel formModel formMsg
    }


type alias Model dataModel formModel formMsg =
    { config: Configuration dataModel formModel formMsg
    , currentRoute: Route
    , currentState: State dataModel formModel
    , indexModel: Maybe (ModelList.Model dataModel)
    , toasts : List Toast.Model
    }


-- Routing
crudRoutes: String -> Parser (Route -> parentRoutes) parentRoutes
crudRoutes name =
    Parser.oneOf
        [ Parser.map Index (Parser.s name)
        , Parser.map Create (Parser.s name </> Parser.s "create")
        , Parser.map Update (Parser.s name </> int)
        ]



-- Transforms a route into a full string
routeToString: String -> Route -> List String
routeToString name route =
    case route of
        Index ->
            [ name ]

        Create ->
            [ name, "create" ]

        Update id ->
            [ name, String.fromInt id ]


configure: String -> String -> RouteGroup -> Decoder (GenericModel dataModel) -> List Expands.Expand
    -> ModelList.Configuration dataModel -> ModelForm.Configuration dataModel formModel formMsg
    -> Configuration dataModel formModel formMsg
configure resourceName apiUrl routeGroup decoder expands indexConfiguration formConfiguration =
    { resourceName = resourceName
    , apiUrl = apiUrl
    , routeGroup = routeGroup
    , decoder = decoder
    , expands = expands
    , indexConfiguration = indexConfiguration
    , formConfiguration = formConfiguration
    }


formRootConfiguration: Configuration dataModel formModel formMsg -> ModelForm.RootConfiguration dataModel
formRootConfiguration configuration =
    ModelForm.configureRoot configuration.resourceName configuration.apiUrl configuration.routeGroup configuration.decoder configuration.expands


-- State manipulation below

initialState : Configuration dataModel formModel formMsg -> Model dataModel formModel formMsg
initialState config  =
    { config = config
    , currentRoute = Index
    , currentState = Inactive
    , indexModel = Nothing
    , toasts = []
    }


replaceFormModel: Model formMsg formModel dataModel -> ModelForm.Model dataModel formModel -> Model formMsg formModel dataModel
replaceFormModel model formModel =
    { model
        | currentState = FormActive formModel
    }


replaceIndexModel: Model dataModel formModel formMsg -> ModelList.Model dataModel -> Model dataModel formModel formMsg
replaceIndexModel model indexModel =
    { model
        | indexModel = Just indexModel
        , currentState = IndexActive indexModel
    }


replaceRoute: Model dataModel formModel formMsg -> Route -> Model dataModel formModel formMsg
replaceRoute model currentRoute =
    { model
        | currentRoute = currentRoute
    }


changePage : Navigation.Key -> Token -> Route
    -> Model dataModel formModel formMsg -> Configuration dataModel formModel formMsg
    -> (Model dataModel formModel formMsg, Cmd (Msg dataModel formMsg))
changePage navKey token route model configuration =
    let
        modelWithRoute = replaceRoute model route
    in
    case route of
        Index ->
            case modelWithRoute.indexModel of
                -- if we were already at the index we will simply use that state, and reload the page data
                Just indexModel ->
                    ( { modelWithRoute
                        | currentRoute = Index
                        , currentState = IndexActive indexModel
                    }
                    , Cmd.map IndexMsg
                        <| ModelList.reload token model.config.indexConfiguration indexModel
                    )

                Nothing ->
                    ModelList.initialState navKey token modelWithRoute.config.indexConfiguration
                        |> Tuple.mapFirst (replaceIndexModel model)
                        |> Tuple.mapSecond (Cmd.map IndexMsg)

        Update id ->
            ModelForm.initialState configuration.formConfiguration (formRootConfiguration configuration) token (Just id)
                |> Tuple.mapFirst (replaceFormModel modelWithRoute)
                |> Tuple.mapSecond (Cmd.map FormMsg)

        Create ->
            ModelForm.initialState configuration.formConfiguration (formRootConfiguration configuration) token Nothing
                |> Tuple.mapFirst (replaceFormModel modelWithRoute)
                |> Tuple.mapSecond (Cmd.map FormMsg)


update: Token -> Msg dataModel formMsg -> Model dataModel formModel formMsg
        -> (Model dataModel formModel formMsg, Cmd (Msg dataModel formMsg))
update token msg model =
    case ( msg, model.currentState ) of
        (FormMsg subMsg, FormActive formModel) ->
            ModelForm.update token model.config.formConfiguration subMsg formModel
                |> Tuple.mapFirst (replaceFormModel model)
                |> Tuple.mapSecond (Cmd.map FormMsg)

        (IndexMsg subMsg, IndexActive indexModel) ->
            ModelList.update token model.config.indexConfiguration subMsg indexModel
                |> Tuple.mapFirst (replaceIndexModel model)
                |> Tuple.mapSecond (Cmd.map IndexMsg)

        (_, _) ->
            (model, Cmd.none)



-- View functions

view : Model dataModel formModel formMsg -> Html (Msg dataModel formMsg)
view model =
    case (model.currentState, model.currentRoute) of
        (IndexActive indexModel, Index) ->
            Html.map IndexMsg
                <| ModelList.view model.config.indexConfiguration indexModel

        (FormActive formModel, Create) ->
            Html.map FormMsg
                <| ModelForm.view "Create" model.config.formConfiguration formModel

        (FormActive formModel, Update _) ->
            Html.map FormMsg
                <| ModelForm.view "Update" model.config.formConfiguration formModel

        _ ->
            Html.div []
                [ Html.h1 [] [text "Inactive State"]
                , Html.p [] [text "Tell Bryce, and try refreshing the page"]
                ]
