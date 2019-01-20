-- Module for the article model
module Athenia.Models.Wiki.Article exposing(..)

import Athenia.Models.Page as Page
import Athenia.Models.User.User as User
import Athenia.Models.Wiki.Iteration as Iteration
import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode


type alias Model =
    { id : Int
    , title : String
    , content : String
    , created_by : Maybe User.Model
    , iterations : List Iteration.Model
    }


type alias ArticlePage
    = Page.Model Model


-- Used for when creating a article
type alias CreateModel =
    { title : String
    , created_by : User.Model
    }


initCreateModel : String -> User.Model -> CreateModel
initCreateModel title user =
    { title = title
    , created_by = user
    }


-- Converts a user model into a JSON string
toCreateJson : CreateModel -> Encode.Value
toCreateJson model =
    Encode.object
        [ ("title", Encode.string model.title)
        , ("created_by_id", Encode.int model.created_by.id)
        ]


-- Decodes a article model retrieved through the API
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "title" string
        |> optional "content" string ""
        |> optional "created_by" (maybe User.modelDecoder) Nothing
        |> optional "iterations" Iteration.listDecoder []


listDecoder : Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder


pageDecoder : Decoder ArticlePage
pageDecoder =
    Page.modelDecoder listDecoder
