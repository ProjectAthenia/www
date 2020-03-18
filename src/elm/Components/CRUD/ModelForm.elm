module Components.CRUD.ModelForm exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Components.LoadingIndicator as LoadingIndicator
import Components.Toast as Toast
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit)


view : String -> msg -> Bool -> List Toast.Model -> List (Html msg) -> Html msg
view title submitMsg loading toasts fields =
    div [ class "model_form" ]
        [ h1 [] [ text title ]
        , Form.form
            [ onSubmit submitMsg ]
                <| List.append fields [submitButton loading]
        , LoadingIndicator.view loading
        , Toast.view toasts
        ]


submitButton : Bool -> Html msg
submitButton loading =
    Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ disabled loading ]
        ]
        [ text "Save" ]
