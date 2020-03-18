module Components.Toast exposing (..)

import Bootstrap.Alert as Alert
import Html exposing (..)
import Html.Attributes exposing (..)
import Process
import Task


-- All type declarations

type Level
    = Success
    | Error


type alias Model =
    { level: Level
    , message: String
    }

type alias Container parentModel =
    { parentModel | toasts: List Model }


appendToast: (Model, Cmd msg) -> Container parentModel -> (Container parentModel, Cmd msg)
appendToast (toast, cmd) parent =
    ( { parent
        | toasts = List.append parent.toasts [toast]
    }
    , cmd
    )


-- Creates a toast model properly
createToast: Level -> (Model -> msg) -> String -> (Model, Cmd msg)
createToast level removalMsg message =
    let
        model =
            { level = level
            , message = message
            }
    in
    ( model
    , Process.sleep (6 * 1000)
        |> Task.perform (\_ -> removalMsg model)
    )


-- View stuff

-- Creates the html for all currently active toasts
view: List Model -> Html msg
view state =
    div [ class "notification_container" ]
        <| List.map buildToast state


-- Builds the html for an individual toast
buildToast: Model -> Html msg
buildToast instance =
    case instance.level of
        Success ->
            Alert.simpleSuccess [] [ text instance.message ]

        Error ->
            Alert.simpleDanger [] [ text instance.message ]
