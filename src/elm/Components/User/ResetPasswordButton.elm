module Components.User.ResetPasswordButton exposing (..)

import Api exposing (Token)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Bootstrap.Button as Button
import Components.LoadingIndicator as LoadingIndicator
import Components.Toast as Toast
import Html exposing (Html, div, text)
import Http
import List.Extra as ListExtra
import Models.Status as Status
import Models.User.User as User


type alias Model =
    { apiUrl: String
    , email: String
    , isLoading: Bool
    , toasts: List Toast.Model
    }


type Msg
    = SendResetEmail
    | RemoveToast Toast.Model
    | ForgotEmailResponse (Result Api.Error Status.Model)


initialModel : String -> String -> Model
initialModel apiUrl email =
    { apiUrl = apiUrl
    , email = email
    , isLoading = False
    , toasts = []
    }


update : Token -> Msg -> Model -> (Model, Cmd Msg)
update token msg model =
    case msg of
        SendResetEmail ->
            ( { model
                | isLoading = True
            }
            , updateSubscriptionRecurringStatus token model
            )

        RemoveToast toast ->
            ( { model
                | toasts = ListExtra.remove toast model.toasts
            }
            , Cmd.none
            )

        ForgotEmailResponse (Ok _) ->
            Toast.appendToast
                (Toast.createToast Toast.Success RemoveToast "Password Reset Email Sent")
                { model | isLoading = False }

        ForgotEmailResponse (Err _) ->
            Toast.appendToast
                (Toast.createToast Toast.Success RemoveToast "Error Sending Password Reset Email")
                { model | isLoading = False }


view : Model -> Html Msg
view model =
    div []
        [ Button.button [ Button.large, Button.info, Button.onClick SendResetEmail ]
            [ text "Send Reset Password Email"
            ]
        , Toast.view model.toasts
        , LoadingIndicator.view model.isLoading
        ]


forgotPasswordEndpoint : Model -> Endpoint
forgotPasswordEndpoint model =
    Endpoint.forgotPassword model.apiUrl


resetBody : String -> Http.Body
resetBody email =
    Http.jsonBody <| User.forgotPasswordJson email


updateSubscriptionRecurringStatus : Token -> Model -> Cmd Msg
updateSubscriptionRecurringStatus token model =
    Api.post (forgotPasswordEndpoint model) (Just token) (resetBody model.email) Status.decoder ForgotEmailResponse