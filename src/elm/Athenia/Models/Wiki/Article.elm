-- Module for the article model
module Athenia.Models.Wiki.Article exposing(..)

import Athenia.Models.Page as Page
import Athenia.Models.User.User as User
import Athenia.Models.Wiki.Iteration as Iteration
import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias Model =
    { id : Int
    , name : String
    , content : String
    , created_by : Maybe User.Model
    , iterations : List Iteration.Model
    }


type alias ArticlePage
    = Page.Model Model


-- Decodes a article model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "name" string
        |> required "content" string
        |> optional "created_by" (maybe User.modelDecoder) Nothing
        |> optional "iterations" Iteration.listDecoder []


listDecoder : Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder


pageDecoder : Decoder ArticlePage
pageDecoder =
    Page.modelDecoder listDecoder
