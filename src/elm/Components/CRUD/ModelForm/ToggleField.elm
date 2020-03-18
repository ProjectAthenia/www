module Components.CRUD.ModelForm.ToggleField exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import Components.CRUD.ModelForm.Input as Input


view : Input.Model -> Bool -> (Bool -> msg) -> Bool -> Html msg
view model on msg isDisabled =
    label [ class "switch" ]
        [ input
            [ disabled isDisabled
            , checked on
            , type_ "checkbox"
            , onCheck msg
            ]
            []
        , div [ class "slider" ] []
        , div [ class "switch_label" ] [ text model.label ]
        ]
