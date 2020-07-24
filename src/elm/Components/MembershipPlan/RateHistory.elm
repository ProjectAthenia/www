module Components.MembershipPlan.RateHistory exposing (..)

import Api exposing (Token)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Bootstrap.Table as Table
import Html exposing (..)
import Models.MembershipPlan.MembershipPlanRate as MembershipPlanRate
import Models.Page as Page


type alias Model =
    { membershipPlanId: Int
    , apiUrl: String
    , loadedRates: List MembershipPlanRate.Model
    }


type Msg =
    MembershipPlanRatesLoadedResponse (Result Api.Error (Page.Model MembershipPlanRate.Model))


initialModel: Token -> String -> Int -> (Model, Cmd Msg)
initialModel token apiUrl membershipPlanId =
    ( { membershipPlanId = membershipPlanId
      , apiUrl = apiUrl
      , loadedRates = []
    }
    , getMembershipPlanRates token
        <| Endpoint.membershipPlanRates apiUrl membershipPlanId 1
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
            <| List.map buildRow model.loadedRates
        }


buildRow : MembershipPlanRate.Model -> Table.Row Msg
buildRow model =
    Table.tr []
        [ Table.td [] [ text <| "$" ++ String.fromFloat model.cost ]
        ]


getMembershipPlanRates : Token -> Endpoint -> Cmd Msg
getMembershipPlanRates token endpoint =
    Api.get endpoint (Just token) (MembershipPlanRate.pageDecoder) MembershipPlanRatesLoadedResponse
