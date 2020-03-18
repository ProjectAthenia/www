module Components.CRUD.ModelForm.NumberField exposing (..)

import Bootstrap.Form.Input as BootstrapInput
import Html exposing (..)
import Html.Attributes exposing (..)
import Components.CRUD.ModelForm.Input as Input


view : Input.Model -> Float -> (String -> msg) -> Bool -> Html msg
view model value msg isDisabled =
    div []
        [ label []
            [ text model.label
            , BootstrapInput.number
                [ BootstrapInput.large
                , BootstrapInput.onInput msg
                , BootstrapInput.disabled isDisabled
                , BootstrapInput.value (String.fromFloat value)
                , BootstrapInput.attrs
                    [ required model.required
                    ]
                ]
            ]
        ]
