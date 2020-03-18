module Components.CRUD.ModelForm.TextField exposing (..)

import Bootstrap.Form.Input as BootstrapInput
import Html exposing (..)
import Html.Attributes exposing (..)
import Components.CRUD.ModelForm.Input as Input


view : Input.Model -> String -> (String -> msg) -> Bool -> Html msg
view model value msg isDisabled =
    div []
        [
            label []
            [ text model.label
            , BootstrapInput.text
                [ BootstrapInput.large
                , BootstrapInput.onInput msg
                , BootstrapInput.disabled isDisabled
                , BootstrapInput.value value
                , BootstrapInput.attrs
                    [ required model.required
                    ]
                ]
            ]
        ]
