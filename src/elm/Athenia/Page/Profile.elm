module Athenia.Page.Profile exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| An Author's profile.
-}

import Athenia.Api as Api exposing (Token)
import Athenia.Api.Endpoint as Endpoint
import Athenia.Components.Loading as Loading
import Athenia.Components.LoadingIndicator as LoadingIndicator
import Athenia.Models.User.User as User
import Athenia.Page as Page
import Athenia.Route as Route
import Athenia.Session as Session exposing (Session)
import Athenia.Utilities.Log as Log
import Athenia.Viewer as Viewer exposing (Viewer)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task exposing (Task)
import Time
import Url.Builder


-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , errors : List String
    , currentTab : ActiveTab
    , token : Token
    , user : Status
    }


type ActiveTab
    = Activity
    | CreatedArticles


type Status
    = Loading Int
    | Loaded User.Model
    | Failed


init : Session -> Token -> Int -> ( Model, Cmd Msg )
init session token userId =
    let
        maybeToken =
            Session.token session
    in
    ( { session = session
      , timeZone = Time.utc
      , errors = []
      , currentTab = defaultFeedTab
      , token = token
      , user = Loading userId
      }
    , Cmd.batch
        [ fetchUser session userId
        , Task.perform GotTimeZone Time.here
        ]
    )


defaultFeedTab : ActiveTab
defaultFeedTab =
    Activity


-- HTTP


fetchUser : Session -> Int -> Cmd Msg
fetchUser session userId =
    let
        maybeToken =
            Session.token session

    in
    Http.send CompletedUserLoad
        <| Api.get (Endpoint.userActivity userId) maybeToken User.modelDecoder


-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    let
        title =
            case (Session.user model.session, model.user) of
                (Just loggedInUser, Loaded user) ->
                    if user.id == loggedInUser.id then
                        "My Profile"
                    else
                        user.name

                (_, Loaded user) ->
                    user.name

                _ ->
                    "Loading"
    in
    { title = title
    , content =
        case model.user of
            Loaded user ->
                div [ id "profile", class "page" ]
                    [ Page.viewErrors ClickedDismissErrors model.errors
                    , div [ class "user-info" ]
                        [ Grid.container []
                            [ Grid.row []
                                [ Grid.col [ Col.xs12, Col.md10 ]
                                    [ h4 [] [ text title ]
                                    ]
                                ]
                            ]
                        ]
                    , Grid.container []
                        [ Grid.row []
                            [ Grid.col [ Col.xs12, Col.md10 ]
                                [viewTabs model.currentTab]
                            ]
                        ]
                    ]

            Loading userId ->
                LoadingIndicator.view True

            Failed ->
                Loading.error "profile"
    }



-- TABS


viewTabs : ActiveTab -> Html Msg
viewTabs tab =
    div [ id "activity-toggle" ]
        <| List.map (viewTab tab)
            [ ("Activity", Activity)
            , ("Created Articles", CreatedArticles)
            ]


viewTab : ActiveTab -> (String, ActiveTab) -> Html Msg
viewTab activeTab (tabName, tab) =
    let
        activeClass =
            if activeTab == tab then
                " btn-primary"
            else
                ""
    in
    button
        [ class ("btn" ++ activeClass)
        , onClick (ClickedTab tab)
        ]
        [ text tabName ]



-- UPDATE


type Msg
    = ClickedDismissErrors
    | ClickedTab ActiveTab
    | CompletedUserLoad (Result Http.Error User.Model)
    | GotTimeZone Time.Zone
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        ClickedTab tab ->
            ( { model | currentTab = tab }
            , Cmd.none
            )

        CompletedUserLoad (Ok user) ->
            ( { model
                | user = Loaded user
            }
            , Cmd.none
            )

        CompletedUserLoad (Err err ) ->
            ( { model | user = Failed }
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



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session