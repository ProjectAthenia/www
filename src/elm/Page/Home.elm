module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Api as Api exposing (Token)
import Components.LoadingIndicator as LoadingIndicator
import Models.User.User as User
import Route as Route
import Session as Session exposing (Session)
import Viewer as Viewer
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
    }



-- Passes in the current session, and requires the unwrapping of the token
init : Session -> String -> Token -> User.Model -> ( Model, Cmd Msg )
init session apiUrl token user =
    ( { token = token
      , user = user
      , showLoading = False
      , session = session
      , apiUrl = apiUrl
      , timeZone = Time.utc
      }
    , Cmd.batch
        [ Task.perform GotTimeZone Time.here
        ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        div [ id "home", class "page" ]
            [ viewBanner
            -- put some page content here

            , LoadingIndicator.view model.showLoading
            -- Any modals should be put here

            ]
    }



viewBanner : Html Msg
viewBanner =
    div [ id "banner" ]
        [ h1 [ class "logo-font" ] [ text "Welcome" ]
        -- Put all of your custom content below
        ]


-- UPDATE


type Msg
    = GotTimeZone Time.Zone
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    , Cmd.none
                    )



-- HTTP




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session