module Athenia.Page.Article.Viewer exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| Viewing an individual article.
-}

import Athenia.Api as Api exposing (Token)
import Athenia.Components.Loading as Loading
import Athenia.Components.LoadingIndicator as LoadingIndicator
import Athenia.Models.Wiki.Article as Article
import Athenia.Page as Page
import Athenia.Route as Route
import Athenia.Session as Session exposing (Session)
import Athenia.Utilities.Log as Log
import Athenia.Viewer as Viewer exposing (Viewer)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Markdown
import Task exposing (Task)
import Time


-- MODEL


type alias Model =
    { session : Session
    , token : Token
    , timeZone : Time.Zone
    , errors : List String

    -- Loaded independently from server
    , article : Status
    }


type Status
    = Loading
    | Loaded Article.Model
    | Failed



init : Session -> Token -> Int -> ( Model, Cmd Msg )
init session token articleId =
    ( { session = session
      , token = token
      , timeZone = Time.utc
      , errors = []
      , article = Loading
      }
    , Cmd.batch
        [ fetchArticle token articleId
        , Task.perform GotTimeZone Time.here
        ]
    )


-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    case model.article of
        Loaded article ->
            { title = article.title
            , content =
                div [ id "article-viewer", class "page" ]
                    [ div [ class "banner" ]
                        [ Grid.container []
                            [ h1 [] [ text article.title ]
                            , div [ class "article-meta" ]
                                <| viewArticleMeta article
                            , Page.viewErrors ClickedDismissErrors model.errors
                            ]
                        ]
                    , Grid.container []
                        [ Grid.row []
                            [ Grid.col [Col.md12]
                                [ Markdown.toHtml [] article.content ]
                            ]
                        ]
                    ]
            }

        Loading ->
            { title = "Article", content = LoadingIndicator.view True }

        Failed ->
            { title = "Article", content = Loading.error "article" }


viewArticleMeta : Article.Model -> List (Html Msg)
viewArticleMeta article =
    List.append
        (case article.created_by of
            Just user ->
                [ p []
                    [ text "Created by "
                    , a [ Route.href (Route.Profile user.id) ]
                        [ text user.name ]
                    ]
                ]
            Nothing ->
                []
        ) [ editButton article ]


-- UPDATE


type Msg
    = ClickedDismissErrors
    | CompletedLoadArticle (Result Api.Error Article.Model)
    | GotTimeZone Time.Zone
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        CompletedLoadArticle (Ok article) ->
            ( { model | article = Loaded article }, Cmd.none )

        CompletedLoadArticle (Err error) ->
            ( { model | article = Failed }
            , Log.error
            )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            case Viewer.maybeToken (Session.viewer session) of
                Just token ->
                    ( { model
                        | session = session
                        , token = token
                    }
                    , Cmd.none
                    )
                Nothing ->
                    ( model
                    , Route.replaceUrl (Session.navKey session) Route.Login
                    )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


-- HTTP


fetchArticle : Token -> Int -> Cmd Msg
fetchArticle token articleId =
    Api.getArticle token articleId CompletedLoadArticle


-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


-- INTERNAL


editButton : Article.Model -> Html Msg
editButton article =
    a [ class "btn btn-outline-secondary btn-sm", Route.href (Route.EditArticle article.id) ]
        [ i [ class "ion-edit" ] [], text " Edit Article" ]