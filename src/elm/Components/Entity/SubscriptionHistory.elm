module Components.Entity.SubscriptionHistory exposing (..)

import Api exposing (Token)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Bootstrap.Grid as Table
import Bootstrap.Table as Table
import Html exposing (Html, div, h2, text)
import Models.MembershipPlan.Subscription as Subscription
import Models.Page as Page
import Task
import Time exposing (Posix, Zone)
import Utilities.DateHelpers as DateHelpers


type alias Model =
    { apiUrl: String
    , entityType: String
    , entityId: Int
    , subscriptionHistory: List Subscription.Model
    , currentSubscription: Maybe Subscription.Model
    , now: Maybe Posix
    , timeZone: Maybe Zone
    }


type Msg
    = SubscriptionHistoryLoadedResponse (Result Api.Error (Page.Model Subscription.Model))
    | GotTime Posix
    | GotTimeZone Zone


initialModel: Token -> String -> String -> Int -> (Model, Cmd Msg)
initialModel token apiUrl entityType entityId =
    ( { apiUrl = apiUrl
      , entityType = entityType
      , entityId = entityId
      , subscriptionHistory = []
      , currentSubscription = Nothing
      , now = Nothing
      , timeZone = Nothing
      }
    , Cmd.batch
        [ getSubscriptionHistory token
            <| Endpoint.entitySubscriptions apiUrl entityType entityId 1
        , Task.perform GotTime Time.now
        , Task.perform GotTimeZone Time.here
        ]
    )


determineCurrentSubscription: Model -> Model
determineCurrentSubscription model =
    case model.now of
        Just now ->
            { model
                | currentSubscription = Subscription.getCurrentSubscription now model.subscriptionHistory
            }

        Nothing ->
            model


update: Token -> Msg -> Model -> (Model, Cmd Msg)
update token msg model =
    case msg of
        SubscriptionHistoryLoadedResponse (Ok page) ->
            let
                updated =
                    { model
                        | subscriptionHistory = model.subscriptionHistory ++ page.data
                    }
            in
            ( determineCurrentSubscription updated
            , case Page.nextPageNumber page of
                Just nextPage ->
                    getSubscriptionHistory token
                        <| Endpoint.entitySubscriptions model.apiUrl model.entityType model.entityId nextPage

                Nothing ->
                    Cmd.none
            )

        SubscriptionHistoryLoadedResponse (Err _) ->
            (model, Cmd.none)

        GotTime now ->
            let
                updated =
                    { model
                        | now = Just now
                    }
            in
            ( determineCurrentSubscription updated
            , Cmd.none
            )

        GotTimeZone timeZone ->
            ( { model
                | timeZone = Just timeZone
            }
            , Cmd.none
            )


viewSubscriptionHistory: Model -> Html Msg
viewSubscriptionHistory model =
    div []
        [ h2 [] [ text "Subscription History" ]
        , Table.table
            { options = [ Table.bordered, Table.striped ]
            , thead = Table.thead []
                [ Table.tr []
                    [ Table.th [] [ text "Subscription Name" ]
                    , Table.th [] [ text "Original Subscription Date" ]
                    , Table.th [] [ text "Expiration Date" ]
                    ]
                ]
            , tbody = Table.tbody []
                <| case model.timeZone of
                    Just timeZone ->
                        List.map (buildRow timeZone) model.subscriptionHistory
                    Nothing ->
                        []
            }
        ]

buildRow: Zone -> Subscription.Model -> Html Msg
buildRow timeZone subscription =
    Table.row []
        [ Table.col []
            [ text <| Subscription.subscriptionName subscription ]
        , Table.col []
            [ text <| DateHelpers.format timeZone subscription.subscribed_at ]
        ,  Table.col []
            [ text
                <| case subscription.expires_at of
                    Just expiresAt ->
                        DateHelpers.format timeZone expiresAt
                    Nothing ->
                        "Does not Expire"
            ]
        ]


getSubscriptionHistory : Token -> Endpoint -> Cmd Msg
getSubscriptionHistory token endpoint =
    Api.get endpoint (Just token) (Subscription.pageDecoder) SubscriptionHistoryLoadedResponse