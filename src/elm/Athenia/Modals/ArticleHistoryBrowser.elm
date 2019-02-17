module Athenia.Modals.ArticleHistoryBrowser exposing (..)

import Athenia.Api as Api exposing (Token)
import Athenia.Components.LoadingIndicator as LoadingIndicator
import Athenia.Models.Wiki.Iteration as Iteration
import Athenia.Session as Session exposing (..)
import Bootstrap.Modal as Modal
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
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
    li [ class "iteration_preview" ] [ text (getIterationPreviewText iteration) ]


getIterationPreviewText : Iteration.Model -> String
getIterationPreviewText iteration =
    case iteration.created_by of
        Just createdBy ->
            "Iteration Created By " ++ createdBy.name ++ " at " ++ (Iteration.formatCreatedAt iteration)
        Nothing ->
            "Iteration Created By Unknown User at " ++ (Iteration.formatCreatedAt iteration)


type Msg
    = Cancel
    | CompletedArticleIterationLoad (Result Http.Error Iteration.Page)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Cancel ->
            ( hide model
            , Cmd.none
            )

        CompletedArticleIterationLoad (Ok page) ->
            let
                anotherPage = page.current_page < page.last_page
                updatedModel = processIterations model page.data
            in
            ( { updatedModel
                | showLoading = anotherPage

            }
            , if anotherPage then
                loadIterations model.token model.articleId (page.current_page + 1)
            else
                Cmd.none
            )

        CompletedArticleIterationLoad (Err error) ->
            ( { model
                | showLoading = False
            }
            , Cmd.none
            )


determineIfIterationIsSameSession : Iteration.Model -> Iteration.Model -> Bool
determineIfIterationIsSameSession newestIteration previousIteration =
    if Iteration.areIterationsTheSameUser newestIteration previousIteration then
        posixToMillis newestIteration.created_at < (posixToMillis previousIteration.created_at) + (60 * 60 * 1000)
    else
        False


checkNextIteration : List Iteration.Model -> List Iteration.Model -> List Iteration.Model
checkNextIteration currentSessions nextIterations =
    case (List.head (List.reverse currentSessions), List.head nextIterations) of
        (Just lastIteration, Just nextIteration) ->
            let
                updatedSessions =
                    if determineIfIterationIsSameSession lastIteration nextIteration then
                        currentSessions
                    else
                        List.append currentSessions [nextIteration]
            in
                checkNextIteration updatedSessions (List.drop 1 nextIterations)
        (_, Just nextIteration) ->
            [nextIteration]

        _ ->
            currentSessions


processIterations : Model -> List Iteration.Model -> Model
processIterations model iterations =
    let
        groupedSessions = model.groupedSessions
    in
    { model
        | iterations = List.append model.iterations iterations
        , groupedSessions = checkNextIteration groupedSessions iterations
    }


hide : Model -> Model
hide model =
    { model
        | visibility = Modal.hidden
    }


-- HTTP


loadIterations : Token -> Int -> Int -> Cmd Msg
loadIterations token articleId page =
    Http.send CompletedArticleIterationLoad
        <| Api.viewArticleIterations token articleId page