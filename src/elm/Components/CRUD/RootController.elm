module Components.CRUD.RootController exposing (..)

import Api exposing (Token)
import Browser.Navigation as Navigation
import Html exposing (..)
import Components.CRUD.ModelList as ModelList
import Url.Parser as Parser exposing ((</>), Parser, int)


type Route
    = Create
    | Index
    | Update Int


type State formModel dataModel
    = Inactive
    | IndexActive (ModelList.Model dataModel)
    | FormActive formModel


type Msg formMsg dataModel
    = FormMsg formMsg
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


type alias Model formMsg formModel dataModel =
    { currentRoute: Route
    , currentState: State formModel dataModel
    , resourceName: String
    , formInit: FormInit formModel formMsg
    , formUpdate: FormUpdate formMsg formModel
    , formView: FormView formModel formMsg
    , indexModel: Maybe (ModelList.Model dataModel)
    , indexConfiguration: ModelList.Configuration dataModel
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


-- State manipulation below

initialState : String -> FormInit formModel formMsg -> FormUpdate formMsg formModel -> FormView formModel formMsg
    -> ModelList.Configuration dataModel -> Model formMsg formModel dataModel
initialState resourceName formInit formUpdate formView indexConfiguration =
    { currentRoute = Index
    , currentState = Inactive
    , resourceName = resourceName
    , formInit = formInit
    , formUpdate = formUpdate
    , formView = formView
    , indexModel = Nothing
    , indexConfiguration = indexConfiguration
    }


replaceFormModel: Model formMsg formModel dataModel -> formModel -> Model formMsg formModel dataModel
replaceFormModel model formModel =
    { model
        | currentState = FormActive formModel
    }


replaceIndexModel: Model formMsg formModel dataModel -> ModelList.Model dataModel -> Model formMsg formModel dataModel
replaceIndexModel model indexModel =
    { model
        | indexModel = Just indexModel
        , currentState = IndexActive indexModel
    }


replaceRoute: Model formMsg formModel dataModel -> Route -> Model formMsg formModel dataModel
replaceRoute model currentRoute =
    { model
        | currentRoute = currentRoute
    }


changePage : Navigation.Key -> Token -> Route -> Model formMsg formModel dataModel -> (Model formMsg formModel dataModel, Cmd (Msg formMsg dataModel))
changePage navKey token route model =
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
                        <| ModelList.reload token model.indexConfiguration indexModel
                    )

                Nothing ->
                    ModelList.initialState navKey token modelWithRoute.indexConfiguration
                        |> Tuple.mapFirst (replaceIndexModel model)
                        |> Tuple.mapSecond (Cmd.map IndexMsg)

        Update id ->
            modelWithRoute.formInit token (Just id)
                |> Tuple.mapFirst (replaceFormModel modelWithRoute)
                |> Tuple.mapSecond (Cmd.map FormMsg)

        Create ->
            modelWithRoute.formInit token Nothing
                |> Tuple.mapFirst (replaceFormModel modelWithRoute)
                |> Tuple.mapSecond (Cmd.map FormMsg)


update: Token -> Msg formMsg dataModel -> Model formMsg formModel dataModel
        -> (Model formMsg formModel dataModel, Cmd (Msg formMsg dataModel))
update token msg model =
    case ( msg, model.currentState ) of
        (FormMsg subMsg, FormActive formModel) ->
            model.formUpdate token subMsg formModel
                |> Tuple.mapFirst (replaceFormModel model)
                |> Tuple.mapSecond (Cmd.map FormMsg)

        (IndexMsg subMsg, IndexActive indexModel) ->
            ModelList.update token model.indexConfiguration subMsg indexModel
                |> Tuple.mapFirst (replaceIndexModel model)
                |> Tuple.mapSecond (Cmd.map IndexMsg)

        (_, _) ->
            (model, Cmd.none)



-- View functions

view : Model formMsg formModel dataModel -> Html (Msg formMsg dataModel)
view model =
    case (model.currentState, model.currentRoute) of
        (IndexActive indexModel, Index) ->
            Html.map IndexMsg
                <| ModelList.view model.indexConfiguration indexModel

        (FormActive formModel, Create) ->
            Html.map FormMsg
                <| model.formView ("Create " ++ model.resourceName) formModel

        (FormActive formModel, Update _) ->
            Html.map FormMsg
                <| model.formView ("Update " ++ model.resourceName) formModel

        _ ->
            Html.div []
                [ Html.h1 [] [text "Inactive State"]
                , Html.p [] [text "Tell Bryce, and try refreshing the page"]
                ]
