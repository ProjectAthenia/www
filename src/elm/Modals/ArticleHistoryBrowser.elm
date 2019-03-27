module Modals.ArticleHistoryBrowser exposing (..)

import Api as Api exposing (Token)
import Components.LoadingIndicator as LoadingIndicator
import Models.Wiki.Iteration as Iteration
import Session as Session exposing (..)
import Bootstrap.Modal as Modal
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)


type alias Model =
    { articleId : Int
    , session : Session
    , token : Token
    , visibility : Modal.Visibility
    , showLoading : Bool
    , iterations : List Iteration.Model
    , groupedSessions : List Iteration.Model
    }


init : Int -> Session -> Token -> Model
init articleId session token =
    { articleId = articleId
    , session = session
    , token = token
    , visibility = Modal.hidden
    , showLoading = False
    , iterations = []
    , groupedSessions = []
    }


initLoad : Model -> (Model, Cmd Msg)
initLoad model =
    ( { model
        | showLoading = True
        , visibility = Modal.shown
        , iterations = []
        , groupedSessions = []
    }
    , loadIterations model.token model.articleId 1
    )


-- Create the view for the article creation modal
view : Model -> Html Msg
view model =
    div []
        [ Modal.config Cancel
            |> Modal.large
            |> Modal.h4 [] [ text "Article History" ]
            |> Modal.body []
                [ ul [] (List.map viewSessionPreview model.groupedSessions)]
            |> Modal.view model.visibility
        , LoadingIndicator.view model.showLoading
        ]


viewSessionPreview : Iteration.Model -> Html Msg
viewSessionPreview iteration =
    li [ class "iteration_preview" ]
        [ text (getIterationPreviewText iteration)
        , span [ onClick (ViewIteration iteration), class "iteration-history-link" ]
            [ text "View Iteration"]
        ]


getIterationPreviewText : Iteration.Model -> String
getIterationPreviewText iteration =
    case iteration.created_by of
        Just createdBy ->
            "Iteration Created By " ++ createdBy.name ++ " at " ++ (Iteration.formatCreatedAt iteration) ++ " "
        Nothing ->
            "Iteration Created By Unknown User at " ++ (Iteration.formatCreatedAt iteration) ++ " "


type Msg
    = Cancel
    | ViewIteration Iteration.Model
    | CompletedArticleIterationLoad (Result Api.Error Iteration.Page)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Cancel ->
            ( hide model
            , Cmd.none
            )

        ViewIteration _ ->
            ( hide model
            , Cmd.none
            )

        CompletedArticleIterationLoad (Ok page) ->
            let
                anotherPage = page.current_page < page.last_page
                updatedModel = mergeNewIterations model page.data
            in
            ( { updatedModel
                | showLoading = anotherPage

            }
            , if anotherPage then
                loadIterations model.token model.articleId (page.current_page + 1)
            else
                Cmd.none
            )

        CompletedArticleIterationLoad (Err _) ->
            ( { model
                | showLoading = False
            }
            , Cmd.none
            )


-- Takes in two iterations to figure out if two iterations are within the same session,
-- meaning that they are from the user and within the same hour.
determineIfIterationIsSameSession : Iteration.Model -> Iteration.Model -> Bool
determineIfIterationIsSameSession iterationA iterationB =
    if Iteration.areIterationsTheSameUser iterationA iterationB then
        abs (posixToMillis iterationA.created_at - (posixToMillis iterationB.created_at)) <= (60 * 60 * 1000)
    else
        False


-- Groups
groupIterations : List Iteration.Model -> List Iteration.Model -> List Iteration.Model
groupIterations existingGroupings newIterations =
    case (List.head (List.reverse existingGroupings), List.head newIterations) of
        (Just lastIteration, Just nextIteration) ->
            let
                updatedSessions =
                    if determineIfIterationIsSameSession lastIteration nextIteration then
                        existingGroupings
                    else
                        List.append existingGroupings [nextIteration]
            in
                groupIterations updatedSessions (List.drop 1 newIterations)
        (_, Just nextIteration) ->
            if List.length newIterations > 1 then
                groupIterations [nextIteration] (List.drop 1 newIterations)
            else
                [nextIteration]

        _ ->
            existingGroupings


-- Takes in a new set of iterations, and then merges them into the list of total iterations
-- as well as grouping them based on our iteration algorithm.
mergeNewIterations : Model -> List Iteration.Model -> Model
mergeNewIterations model iterations =
    let
        groupedSessions = model.groupedSessions
    in
    { model
        | iterations = List.append model.iterations iterations
        , groupedSessions = groupIterations groupedSessions iterations
    }


hide : Model -> Model
hide model =
    { model
        | visibility = Modal.hidden
    }


-- HTTP


loadIterations : Token -> Int -> Int -> Cmd Msg
loadIterations token articleId page =
    Api.viewArticleIterations token articleId page CompletedArticleIterationLoad