module Page.ConfigError exposing (..)

import Html exposing (Html)


view : String -> { title : String, content : Html msg }
view message =
    { title = ""
    , content = Html.text message
    }
