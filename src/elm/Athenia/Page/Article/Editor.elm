module Athenia.Page.Article.Editor exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Athenia.Api as Api exposing (Token)
import Athenia.Api.Endpoint as Endpoint
import Athenia.Components.Loading as Loading
import Athenia.Models.Wiki.Article as Article
import Athenia.Page as Page
import Athenia.Route as Route
import Athenia.Session as Session exposing (Session)
import Bootstrap.Form as Form
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Time



-- MODEL


type alias Model =
    { session : Session
    , token : Token
    , article : Status
    }


type
    Status
    -- Edit Article
    = Loading Int
    | LoadingSlowly Int
    | LoadingFailed
    | Editing Article.Model (List Problem) Form


type Problem
    = ServerError String


type alias Form =
    { body : String
    }


init : Session -> Token -> Int -> ( Model, Cmd Msg )
init session token articleId =
    ( { session = session
      , token = token
      , article = Loading articleId
      }
    , Cmd.batch
        [ fetchArticle token articleId
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )


-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title =
        case model.article of
            Editing article _ _ ->
                "Edit Article - " ++ article.title

            _ ->
                "Loading"

    , content =
        viewContent model
    }


viewContent : Model -> Html Msg
viewContent model =
    let
        formHtml =
            case model.article of
                Loading _ ->
                    []

                LoadingSlowly _ ->
                    [ Loading.icon ]

                Editing article problems form ->
                    [ viewTitle article
                    , viewProblems problems
                    , viewForm model.token form
                    ]

                LoadingFailed ->
                    [ text "Article failed to load." ]
    in
    div [ id "article-editor", class "page" ]
        [ Grid.container []
            [ Grid.row []
                [ Grid.col [Col.md12]
                    formHtml
                ]
            ]
        ]


viewTitle : Article.Model -> Html msg
viewTitle article =
    h1 [ id "title" ] [ text article.title ]


viewProblems : List Problem -> Html msg
viewProblems problems =
    ul [ class "error-messages" ]
        (List.map viewProblem problems)


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                ServerError message ->
                    message
    in
    li [] [ text errorMessage ]


viewForm : Token -> Form -> Html Msg
viewForm token fields =
    Form.form [ onSubmit (NoAction) ]
        [ h2 [] [ text "Enter the article contents below." ]
        , Textarea.textarea
            [ Textarea.rows 20
            , Textarea.onInput EnteredBody
            , Textarea.value fields.body
            ]
        ]


-- UPDATE


type Msg
    = NoAction
    | EnteredBody String
    | CompletedLoadArticle (Result Http.Error Article.Model)
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoAction ->
            (model, Cmd.none)

        EnteredBody body ->
            updateForm (\form -> { form | body = body }) model

        CompletedLoadArticle (Err err) ->
            ( { model | article = LoadingFailed }
            , Cmd.none
            )

        CompletedLoadArticle (Ok article) ->
            let
                form =
                    { body = article.content
                    }
            in
            ( { model | article = Editing article [] form }
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                status =
                    case model.article of
                        Loading articleId ->
                            LoadingSlowly articleId

                        other ->
                            other
            in
            ( { model | article = status }, Cmd.none )


{-| Helper function for `update`. Updates the form, if there is one,
and returns Cmd.none.

Useful for recording form fields!

This could also log errors to the server if we are trying to record things in
the form and we don't actually have a form.

-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    let
        newModel =
            case model.article of
                Editing article errors form ->
                    { model | article = Editing article errors (transform form) }

                _ ->
                    model

    in
    ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


-- HTTP


fetchArticle : Token -> Int -> Cmd Msg
fetchArticle token articleId =
    Http.send CompletedLoadArticle
        <| Api.article token articleId


-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
