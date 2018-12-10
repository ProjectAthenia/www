-- Module for the user model
module Athenia.Models.Article exposing(..)

import Athenia.Models.Page as Page
import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias Model =
    { id: Int
    , content: String
    }


type alias ArticlePage
    = Page.Model Model


-- Decodes a article model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "content" string


listDecoder : Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder


pageDecoder : Decoder ArticlePage
pageDecoder =
    Page.modelDecoder listDecoder
