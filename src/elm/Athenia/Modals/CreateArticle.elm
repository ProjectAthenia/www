module Athenia.Modals.CreateArticle exposing (..)

import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL


type alias Model =
    { title : String
    , visibility: Modal.Visibility
    }


init : Model
init =
    { title = ""
    , visibility = Modal.hidden
    }


-- Create the view for the article creation modal
view : Model -> Html Msg
view model =
    Modal.config Cancel
        |> Modal.large
        |> Modal.h4 [] [ text "Create Article" ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col
                        []
                        [ Input.text
                            [ Input.large
                            , Input.placeholder "Enter Article Title"
                            , Input.onInput EnteredTitle
                            , Input.value model.title
                            , Input.attrs [required True]
                            ]
                        ]
                    ]
                , Grid.row [ Row.attrs [class "button-wrapper"] ]
                    [ Grid.col
                        [ Col.xs6 ]
                        [ Button.button
                            [ Button.info
                            , Button.onClick Cancel
                            ]
                            [ text "Cancel" ]
                        ]
                    , Grid.col
                        [ Col.xs6 ]
                        [ Button.button
                            [ Button.danger
                            , Button.onClick Confirm
                            ]
                            [ text "Submit" ]
                        ]
                    ]
                ]
            ]
        |> Modal.view model.visibility



-- UPDATE


type Msg
    = EnteredTitle String
    | Cancel
    | Confirm


update: Msg -> Model -> Model
update msg model =
    case msg of
        EnteredTitle title ->
            { model
                | title = title
            }

        Cancel ->
            hide model

        Confirm ->
            hide model


hide: Model -> Model
hide model =
    {model
        | visibility = Modal.hidden
    }


show: Model -> Model
show model =
    {model
        | visibility = Modal.shown
    }
