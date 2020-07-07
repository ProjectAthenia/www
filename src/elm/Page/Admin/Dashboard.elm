module Page.Admin.Dashboard exposing (..)

import Api exposing (Token)
import Browser.Navigation as Navigation
import Components.CRUD.RootController as RootController
import Page.Admin.Sections.MembershipPlan as MembershipPlan
import Url.Parser as Parser exposing ((</>), Parser, top)


type Route
    = Dashboard
    | MembershipPlanRoute RootController.Route


type Msg
    = MembershipPlanMsg MembershipPlan.Msg


type alias Model =
    { currentPage: Route
    , history: List (Route)
    , membershipPlanModel: MembershipPlan.Model
    }



initialState: String -> Model
initialState apiUrl =
    { currentPage = Dashboard
    , history = []
    , membershipPlanModel = MembershipPlan.initialModel apiUrl
    }



routes: Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Dashboard (top)
        , Parser.map MembershipPlanRoute (RootController.crudRoutes "membership-plans")
        ]


-- Transforms a route into a full string
routeToString: Route -> String
routeToString route =
    case route of
        Dashboard ->
            "/"

        MembershipPlanRoute subRoute ->
            RootController.routeToString "membership-plans" subRoute


initPage : Navigation.Key -> Token ->  Route -> Model -> (Model, Cmd Msg)
initPage navKey token route model =
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
