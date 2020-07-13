module Components.CRUD.ModelForm.TextAreaField exposing (..)

import Bootstrap.Form.Textarea as TextArea
import Html exposing (..)
import Html.Attributes exposing (..)
import Components.CRUD.ModelForm.Input as Input


view : Input.Model -> String -> (String -> msg) -> Bool -> Html msg
view model value msg isDisabled =
    div [ class "field" ]
        [ label []
            [ text model.label
            , TextArea.textarea
                [ TextArea.rows 4
                , TextArea.onInput msg
                , TextArea.value value
                , TextArea.attrs
                    [ required model.required
                    , disabled isDisabled
                    ]
                ]
            ]
        ]
