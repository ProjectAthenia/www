module Components.Entity.SubscriptionHistory exposing (..)

import Api exposing (Token)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Models.MembershipPlan.Subscription as Subscription
import Models.Page as Page
import Task
import Time exposing (Posix)


type alias Model =
    { apiUrl: String
    , entityType: String
    , entityId: Int
    , subscriptionHistory: List Subscription.Model
    , currentSubscription: Maybe Subscription.Model
    , now: Maybe Posix
    }


type Msg
    = SubscriptionHistoryLoadedResponse (Result Api.Error (Page.Model Subscription.Model))
    | GotTime Posix


initialModel: Token -> String -> String -> Int -> (Model, Cmd Msg)
initialModel token apiUrl entityType entityId =
    ( { apiUrl = apiUrl
      , entityType = entityType
      , entityId = entityId
      , subscriptionHistory = []
      , currentSubscription = Nothing
      , now = Nothing
      }
    , Cmd.batch
        [ getSubscriptionHistory token
            <| Endpoint.entitySubscriptions apiUrl entityType entityId 1
        , Task.perform GotTime Time.now
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


getSubscriptionHistory : Token -> Endpoint -> Cmd Msg
getSubscriptionHistory token endpoint =
    Api.get endpoint (Just token) (Subscription.pageDecoder) SubscriptionHistoryLoadedResponse