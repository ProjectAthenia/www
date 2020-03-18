module Components.AudioPlayer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

view: String -> Html msg
view audioClipUrl =
    audio [ controls True ]
        [ source [ src audioClipUrl ] []
        ]
