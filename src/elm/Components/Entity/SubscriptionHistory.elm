module Components.Entity.SubscriptionHistory exposing (..)

import Api exposing (Token)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Models.MembershipPlan.Subscription as Subscription
import Models.Page as Page


type alias Model =
    { apiUrl: String
    , entityType: String
    , entityId: Int
    , subscriptionHistory: List Subscription.Model
    , currentSubscription: Maybe Subscription.Model
    }


type Msg =
    SubscriptionHistoryLoadedResponse (Result Api.Error (Page.Model Subscription.Model))


initialModel: Token -> String -> String -> Int -> (Model, Cmd Msg)
initialModel token apiUrl entityType entityId =
    ( { apiUrl = apiUrl
      , entityType = entityType
      , entityId = entityId
      , subscriptionHistory = []
      , currentSubscription = Nothing
      }
    , getSubscriptionHistory token
        <| Endpoint.entitySubscriptions apiUrl entityType entityId 1
    )


update: Token -> Msg -> Model -> (Model, Cmd Msg)
update token msg model =
    case msg of
        SubscriptionHistoryLoadedResponse (Ok page) ->
            ( { model
                | subscriptionHistory = model.subscriptionHistory ++ page.data
            }
            , case Page.nextPageNumber page of
                Just nextPage ->
                    getSubscriptionHistory token
                        <| Endpoint.entitySubscriptions model.apiUrl model.entityType model.entityId nextPage

                Nothing ->
                    Cmd.none
            )

        SubscriptionHistoryLoadedResponse (Err _) ->
            (model, Cmd.none)


getSubscriptionHistory : Token -> Endpoint -> Cmd Msg
getSubscriptionHistory token endpoint =
    Api.get endpoint (Just token) (Subscription.pageDecoder) SubscriptionHistoryLoadedResponse