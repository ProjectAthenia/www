-- Component for the view of the loading indicator
module Components.LoadingIndicator exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- Handles displaying the html required for the loading indicator, and displaying it based on state
view : Bool -> Html msg
view showLoading =
    div
        [ class ("spinner " ++
            if showLoading then "visible" else "")
        ]
        [ div [class "bounce1"] []
        , div [class "bounce2"] []
        , div [class "bounce3"] []
        ]
