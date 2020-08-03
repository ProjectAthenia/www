module Components.Entity.Payment exposing (..)

import Api exposing (Token)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Bootstrap.Table as Table
import Components.LoadingIndicator as LoadingIndicator
import Components.Toast as Toast
import Html exposing (Html, div, h2, h3, text)
import Http
import List.Extra as ListExtra
import Modals.Confirmation as Confirmation
import Models.Payment.Payment as Payment
import Models.Page as Page
import Task
import Time exposing (Posix, Zone)


type alias Model =
    { apiUrl: String
    , entityType: String
    , entityId: Int
    , paymentHistory: List Payment.Model
    , timeZone: Maybe Zone
    , toasts: List Toast.Model
    , refundModal: Maybe (Confirmation.Model Msg)
    , isLoading: False
    }


type Msg
    = PaymentHistoryLoadedResponse (Result Api.Error (Page.Model Payment.Model))
    | OpenRefundModel Payment.Model
    | CloseRefundModal
    | RefundPayment Payment.Model
    | PaymentRefundedResponse (Result Api.Error Payment.Model)
    | GotTimeZone Zone


initialModel: Token -> String -> String -> Int -> (Model, Cmd Msg)
initialModel token apiUrl entityType entityId =
    ( { apiUrl = apiUrl
      , entityType = entityType
      , entityId = entityId
      , paymentHistory = []
      , timeZone = Nothing
      , toasts = []
      , refundModal = Nothing
      , isLoading = False
      }
    , Cmd.batch
        [ getPaymentHistory token
            <| Endpoint.entityPayments apiUrl entityType entityId 1
        , Task.perform GotTimeZone Time.here
        ]
    )


update: Token -> Msg -> Model -> (Model, Cmd Msg)
update token msg model =
    case msg of
        PaymentHistoryLoadedResponse (Ok page) ->
            ( { model
                | paymentHistory = model.paymentHistory ++ page.data
            }
            , case Page.nextPageNumber page of
                Just nextPage ->
                    getPaymentHistory token
                        <| Endpoint.entityPayments model.apiUrl model.entityType model.entityId nextPage

                Nothing ->
                    Cmd.none
            )

        PaymentHistoryLoadedResponse (Err _) ->
            -- TODO add toast
            ( model
            , Cmd.none
            )

        PaymentRefundedResponse (Ok payment) ->
            -- TODO add success response
            ( { model
                | paymentHistory =
                    ListExtra.setIf (\i -> payment.id == i.id) payment model.paymentHistory
                , isLoading = True
            }
            , Cmd.none
            )

        PaymentRefundedResponse (Err _) ->
            -- TODO Add toast
            ( { model
                | isLoading = False
            }
            , Cmd.none
            )

        GotTimeZone timeZone ->
            ( { model
                | timeZone = Just timeZone
            }
            , Cmd.none
            )

        OpenRefundModel payment ->
            ( { model
                | refundModal = Just
                    <| Confirmation.initialState "Refund Payment" "Are you sure you want to refund this payment? This cannot be undone." (RefundPayment payment) CloseRefundModal
            }
            , Cmd.none
            )

        CloseRefundModal ->
            ( { model
                | refundModal = Nothing
            }
            , Cmd.none
            )

        RefundPayment payment ->
            ( { model
                | isLoading = True
            }
            , refundPayment token model payment
            )


view : Model -> Html Msg
view model =
    div []
        [ viewPaymentHistory model
        , case model.refundModal of
            Just modal ->
                Confirmation.view modal
            Nothing ->
                text ""
        , Toast.view model.toasts
        , LoadingIndicator.view model.isLoading
        ]


viewPaymentHistory : Model -> Html Msg
viewPaymentHistory model =
    div []
        [ h2 [] [ text "Payment History" ]
        , Table.table
            { options = [ Table.bordered, Table.striped ]
            , thead = Table.thead []
                [ Table.tr []
                    [ ]
                ]
            , tbody = Table.tbody []
                <| case model.timeZone of
                    Just timeZone ->
                        List.map (buildRow timeZone) model.paymentHistory
                    Nothing ->
                        []
            }
        ]

buildRow: Zone -> Payment.Model -> Table.Row Msg
buildRow timeZone payment =
    Table.tr []
        [ ]


getPaymentHistory : Token -> Endpoint -> Cmd Msg
getPaymentHistory token endpoint =
    Api.get endpoint (Just token) (Payment.pageDecoder) PaymentHistoryLoadedResponse


updateEndpoint : Model -> Payment.Model -> Endpoint
updateEndpoint model payment =
    Endpoint.entityPayment model.apiUrl model.entityType model.entityId payment.id


refundUpdateBody : Http.Body
refundUpdateBody =
    Http.jsonBody <| Payment.refundJson


refundPayment : Token -> Model -> Payment.Model -> Cmd Msg
refundPayment token model payment =
    Api.put (updateEndpoint model payment) token refundUpdateBody Payment.modelDecoder PaymentRefundedResponse
