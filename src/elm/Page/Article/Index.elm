module Page.Article.Index exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Api as Api exposing (Token)
import Components.Loading as Loading
import Components.LoadingIndicator as LoadingIndicator
import Modals.CreateArticle as CreateArticleModal
import Models.User.User as User
import Models.Wiki.Article as Article
import Models.Page as PageModel
import Route as Route
import Session as Session exposing (Session)
import Utilities.Log as Log
import Viewer as Viewer
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Html exposing (..)
import Html.Attributes exposing (..)
import Task exposing (Task)
import Time



-- MODEL


type alias Model =
    { token : Token
    , user : User.Model
    , showLoading : Bool
    , apiUrl : String
    , session : Session
    , timeZone : Time.Zone
    , status : Status
    , articles : List Article.Model
    , createArticleModal : CreateArticleModal.Model
    }


type Status
    = Loading
    | LoadingSlowly
    | Loaded
    | Failed


-- Passes in the current session, and requires the unwrapping of the token
init : Session -> String -> Token -> User.Model -> ( Model, Cmd Msg )
init session apiUrl token user =
    ( { token = token
      , user = user
      , showLoading = True
      , session = session
      , apiUrl = apiUrl
      , timeZone = Time.utc
      , status = Loading
      , articles = []
      , createArticleModal = CreateArticleModal.init  user session apiUrl token
      }
    , Cmd.batch
        [ fetchArticles apiUrl token 1
        , Task.perform GotTimeZone Time.here
        ]
    )



-- VIEW


viewBanner : Html Msg
viewBanner =
    div [ id "banner" ]
        [ h2 []
            [ text "All available articles listed below"
            ]
        , Button.button
            [ Button.outlinePrimary
            , Button.onClick OpenCreateArticlePrompt
            ] [ text "Create Article" ]
        ]


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Articles"
    , content =
        div [ id "home", class "page" ]
            [ viewBanner
            , Grid.container []
                [ Grid.row []
                    [ Grid.col [] <|
                        case model.status of
                            Loaded ->
                                [ div [ class "available-articles" ] <|
                                    List.map viewArticle model.articles
                                ]

                            Loading ->
                                []

                            LoadingSlowly ->
                                [ Loading.icon ]

                            Failed ->
                                [ Loading.error "Articles" ]
                    ]
                ]
            , LoadingIndicator.view model.showLoading
            , CreateArticleModal.view model.createArticleModal
                |> Html.map CreateArticleModalMsg
            ]
    }


viewArticle : Article.Model -> Html Msg
viewArticle article =
    div [ class "article" ]
        [ h2 [] [text article.title]
        , a [ Route.href (Route.Article article.id) ] [text "View"]
        ]


-- UPDATE


type Msg
    = CompletedArticlesLoad (Result Api.Error Article.Page)
    | GotTimeZone Time.Zone
    | GotSession Session
    | OpenCreateArticlePrompt
    | CreateArticleModalMsg CreateArticleModal.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedArticlesLoad (Ok articlePage) ->
            ( { model
                | status = Loaded
                , showLoading = (PageModel.nextPageNumber articlePage) /= Nothing
                , articles = List.concat [model.articles, articlePage.data]
            }
            , case PageModel.nextPageNumber articlePage of
                Just pageNumber ->
                    fetchArticles model.apiUrl model.token pageNumber
                Nothing ->
                    Cmd.none
            )

        CompletedArticlesLoad (Err _) ->
            ( { model | status = Failed, showLoading = False }, Log.error )

        OpenCreateArticlePrompt ->
            ( { model
                | createArticleModal =
                    CreateArticleModal.show
                        <| CreateArticleModal.init model.user model.session model.apiUrl model.token
            }
            , Cmd.none
            )

        CreateArticleModalMsg modalMsg ->
            let
                createArticleUpdate = CreateArticleModal.update modalMsg model.createArticleModal
            in
                ({ model
                    | createArticleModal = Tuple.first createArticleUpdate
                }
                , Tuple.second createArticleUpdate
                    |> Cmd.map CreateArticleModalMsg
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



-- HTTP


fetchArticles : String -> Token -> Int -> Cmd Msg
fetchArticles apiUrl token page =
    Api.viewArticles apiUrl token page CompletedArticlesLoad



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session