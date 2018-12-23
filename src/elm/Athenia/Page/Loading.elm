module Athenia.Page.Loading exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "Loading"
    , content
        = Html.div [class "page center-content"]
            [ Html.h1 [] [Html.text "Please Wait"]
            , Html.p [] [Html.text "Getting things ready."]
            ]
    }