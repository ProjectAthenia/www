module Modals.Confirmation exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Html exposing (..)


-- type declarations start below

type alias Model msg =
    { title : String
    , message : String
    , visibility : Modal.Visibility
    , confirmationMsg : msg
    , cancelMsg : msg
    }


-- state change functions

initialState : String -> String -> msg -> msg -> Model msg
initialState title message confirmationMsg cancelMsg =
    { title = title
    , message = message
    , visibility = Modal.hidden
    , confirmationMsg = confirmationMsg
    , cancelMsg = cancelMsg
    }


showModal : Model msg -> Model msg
showModal model =
    { model
        | visibility = Modal.shown
    }


hideModal : Model msg -> Model msg
hideModal model =
    { model
        | visibility = Modal.hidden
    }


-- view stuff starts below

view : Model msg -> Html msg
view model =
    Modal.config model.cancelMsg
        |> Modal.large
        |> Modal.h4 [] [ text model.title ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col [] [text model.message]
                    ]
                , Grid.row []
                    [ Grid.col
                        [ Col.xs6 ]
                        [ Button.button
                            [ Button.info
                            , Button.onClick model.cancelMsg
                            ]
                            [ text "Cancel" ]
                        ]
                    , Grid.col
                        [ Col.xs6 ]
                        [ Button.button
                            [ Button.danger
                            , Button.onClick model.confirmationMsg
                            ]
                            [ text "Confirm" ]
                        ]
                    ]
                ]
            ]
        |> Modal.view model.visibility
