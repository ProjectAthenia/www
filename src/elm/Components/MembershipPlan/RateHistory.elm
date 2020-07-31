module Components.MembershipPlan.RateHistory exposing (..)

import Api exposing (Token)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Bootstrap.Table as Table
import Html exposing (..)
import Models.MembershipPlan.MembershipPlanRate as MembershipPlanRate
import Models.Page as Page
import Task
import Time exposing (Zone)
import Utilities.DateHelpers as DateHelpers


type alias Model =
    { membershipPlanId: Int
    , apiUrl: String
    , loadedRates: List MembershipPlanRate.Model
    , timeZone: Maybe Zone
    }


type Msg
    = MembershipPlanRatesLoadedResponse (Result Api.Error (Page.Model MembershipPlanRate.Model))
    | GotTimeZone Zone


initialModel: String -> Token -> Int -> (Model, Cmd Msg)
initialModel apiUrl token membershipPlanId =
    ( { membershipPlanId = membershipPlanId
      , apiUrl = apiUrl
      , loadedRates = []
      , timeZone = Nothing
    }
    , Cmd.batch
        [ getMembershipPlanRates token
            <| Endpoint.membershipPlanRates apiUrl membershipPlanId 1
        , Task.perform GotTimeZone Time.here
        ]
    )


update: Token -> Msg -> Model -> (Model, Cmd Msg)
update token msg model =
    case msg of
        MembershipPlanRatesLoadedResponse (Ok page) ->
            ( { model
                | loadedRates = model.loadedRates ++ page.data
            }
            , case Page.nextPageNumber page of
                Just nextPage ->
                    getMembershipPlanRates token
                        <| Endpoint.membershipPlanRates model.apiUrl model.membershipPlanId nextPage

                Nothing ->
                    Cmd.none
            )

        MembershipPlanRatesLoadedResponse (Err _) ->
            (model, Cmd.none)

        GotTimeZone timeZone ->
            ( { model
                | timeZone = Just timeZone
            }
            , Cmd.none
            )


view: Model -> Html Msg
view model =
    Table.table
        { options = [ Table.bordered, Table.striped ]
        , thead = Table.thead []
            [ Table.tr []
                [ Table.th [] [ text "Rate History" ]
                ]
            ]
        , tbody = Table.tbody []
            <| case model.timeZone of
                Just timeZone ->
                    List.map (buildRow timeZone) model.loadedRates
                Nothing ->
                    []
        }


buildRow : Zone -> MembershipPlanRate.Model -> Table.Row Msg
buildRow timeZone model =
    Table.tr []
        [ Table.td [] [ text <| buildRateText timeZone model]
        ]


buildRateText : Zone -> MembershipPlanRate.Model -> String
buildRateText timeZone model =
    String.concat
        [ "$" ++ String.fromFloat model.cost
        , " "
        , case model.created_at of
            Just createdAt ->
                "Created " ++ DateHelpers.format timeZone createdAt
            Nothing ->
                "Unknown creation date"
        ]


getMembershipPlanRates : Token -> Endpoint -> Cmd Msg
getMembershipPlanRates token endpoint =
    Api.get endpoint (Just token) (MembershipPlanRate.pageDecoder) MembershipPlanRatesLoadedResponse
