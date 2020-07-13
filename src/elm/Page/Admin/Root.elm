module Page.Admin.Root exposing (..)

import Api exposing (Token)
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Browser.Navigation as Navigation
import Components.CRUD.RootController as RootController
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Admin.Sections.MembershipPlan as MembershipPlan
import Session exposing (Session)
import Url.Parser as Parser exposing ((</>), Parser, top)
import Viewer


type Route
    = Dashboard
    | MembershipPlanRoute RootController.Route


type Msg
    = GotSession Session
    | MembershipPlanMsg MembershipPlan.Msg


type alias Model =
    { session : Session
    , currentPage: Route
    , token : Token
    , history: List (Route)
    , membershipPlanModel: MembershipPlan.Model
    }


initialState: Session -> String -> Token -> Model
initialState session apiUrl token =
    { session = session
    , currentPage = Dashboard
    , token = token
    , history = []
    , membershipPlanModel = MembershipPlan.initialModel apiUrl
    }


routes: Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Dashboard (top)
        , Parser.map MembershipPlanRoute (RootController.crudRoutes "membership-plans")
        ]


routeToHref: Route -> String
routeToHref route =
    "/admin/" ++ (String.join "/" (routeToString route))

-- Transforms a route into a full string
routeToString: Route -> List String
routeToString route =
    case route of
        Dashboard ->
            [ "" ]

        MembershipPlanRoute subRoute ->
            RootController.routeToString "membership-plans" subRoute


changePage : Navigation.Key -> Token -> Route -> Model -> (Model, Cmd Msg)
changePage navKey token route model =
    case route of
        Dashboard ->
            ( { model | currentPage = Dashboard }
            , Cmd.none
            )

        MembershipPlanRoute subRoute ->
            let
                (membershipPlanModel, membershipPlanCmd) =
                    RootController.changePage navKey token subRoute model.membershipPlanModel
            in
            ( { model
                | membershipPlanModel = membershipPlanModel
                , currentPage = MembershipPlanRoute subRoute
            }
            , Cmd.map MembershipPlanMsg membershipPlanCmd
            )


-- Handles all ui actions from the user. Returns a new model, msg, maybe a toast, a list of requests, and a boolean for whether or not to log out
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MembershipPlanMsg subMsg ->
            let
                (membershipPlanModel, membershipPlanMsg) =
                    RootController.update model.token subMsg model.membershipPlanModel
            in
            ( { model | membershipPlanModel = membershipPlanModel }
            , Cmd.map MembershipPlanMsg membershipPlanMsg
            )

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
                    , Navigation.replaceUrl (Session.navKey session) "/login"
                    )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Admin"
    , content = div [ id "admin_wrapper" ]
        <| case model.currentPage of
            Dashboard ->
                dashboard

            MembershipPlanRoute subRoute ->
                List.map (Html.map MembershipPlanMsg)
                    <| RootController.view model.membershipPlanModel
    }

-- Creates the main view for the dashboard view
dashboard : List (Html Msg)
dashboard =
    [ h1 [] [ text "Admin" ]
    , div [ class "dash-squares" ]
        [ buildDataCard "Manage Membership Plan" "Create, edit, delete, and membership plan data." MembershipPlanRoute
        ]
    ]


buildDataCard : String -> String -> (RootController.Route -> Route) -> Html Msg
buildDataCard title information route =
    Card.config [ Card.outlineInfo ]
        |> Card.headerH4 [] [ text title ]
        |> Card.block []
            [ Block.text [] [ text information ]
            , Block.custom <|
                Button.linkButton
                    [ Button.primary, Button.attrs [ href (routeToHref (route RootController.Index)) ] ]
                    [ text "Manage" ]
            ]
        |> Card.view


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Session.changes GotSession (Session.navKey model.session)
        ]


-- EXPORT

toSession : Model -> Session
toSession model =
    model.session