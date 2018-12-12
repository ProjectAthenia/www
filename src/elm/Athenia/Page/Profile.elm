module Athenia.Page.Profile exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| An Author's profile.
-}

import Athenia.Api as Api exposing (Token)
import Athenia.Api.Endpoint as Endpoint
import Athenia.Components.Loading as Loading
import Athenia.Models.User.User as User
import Athenia.Page as Page
import Athenia.Route as Route
import Athenia.Session as Session exposing (Session)
import Athenia.Utilities.Log as Log
import Athenia.Viewer as Viewer exposing (Viewer)
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
    | LoadingSlowly Int
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
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
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
                div [ class "profile-page" ]
                    [ Page.viewErrors ClickedDismissErrors model.errors
                    , div [ class "user-info" ]
                        [ div [ class "container" ]
                            [ div [ class "row" ]
                                [ div [ class "col-xs-12 col-md-10 offset-md-1" ]
                                    [ h4 [] [ text title ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "container" ]
                        [ div [ class "row" ]
                            [ div [ class "col-xs-12 col-md-10 offset-md-1" ]
                                [ div [ class "articles-toggle" ] <|
                                    [ viewTabs model.currentTab ]
                                ]
                            ]
                        ]
                    ]

            Loading userId ->
                text ""

            LoadingSlowly userId ->
                Loading.icon

            Failed ->
                Loading.error "profile"
    }



-- TABS


viewTabs : ActiveTab -> Html Msg
viewTabs tab =
    ul [ class "nav nav-pills outline-active" ]
        <| List.map (viewTab tab)
            [ ("Activity", Activity)
            , ("Created Articles", CreatedArticles)
            ]


viewTab : ActiveTab -> (String, ActiveTab) -> Html Msg
viewTab activeTab (tabName, tab) =
    let
        activeClass =
            if activeTab == tab then
                " active"
            else
                ""
    in
    li [ class "nav-item" ]
        [ -- Note: The RealWorld CSS requires an href to work properly.
          a [ class ("nav-link" ++ activeClass)
            , onClick (ClickedTab tab)
            , href ""
            ]
            [ text tabName ]
        ]



-- UPDATE


type Msg
    = ClickedDismissErrors
    | ClickedTab ActiveTab
    | CompletedUserLoad (Result Http.Error User.Model)
    | GotTimeZone Time.Zone
    | GotSession Session
    | PassedSlowLoadThreshold


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
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        PassedSlowLoadThreshold ->
            case model.user of
                Loading userId ->
                    ( { model | user = LoadingSlowly userId }, Cmd.none )

                _ ->
                    (model, Cmd.none)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session