module Components.CRUD.ModelForm.SelectField exposing (..)

import Bootstrap.Form.Select as Select
import Html exposing (..)
import Html.Attributes as Attributes


type alias Model =
    { options: List (String, String)
    , name: String
    , label: String
    , required: Bool
    }


configure: List (String, String) -> String -> String -> Bool -> Model
configure options name label required =
    { options = options
    , name = name
    , label = label
    , required = required
    }


view: Model -> String -> (String -> msg) -> Bool -> Html msg
view model value msg isDisabled =
    Select.select
        [ Select.onChange msg
        , Select.disabled isDisabled
        ]
        (List.map (viewOption value) model.options)


viewOption: String -> (String, String) -> Select.Item msg
viewOption value (name, label) =
    Select.item [ Attributes.value name, Attributes.selected (value == name) ] [ text label ]
