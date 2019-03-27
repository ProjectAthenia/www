-- Page model for dealing with index results from the API
module Models.Page exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (required)


type alias Model a =
    { total: Int
    , data: List a
    , current_page: Int
    , last_page: Int
    , per_page: Int
    }


modelDecoder: Decoder (List a) -> Decoder (Model a)
modelDecoder modelListDecoder =
    JsonDecode.succeed Model
        |> required "total" int
        |> required "data" modelListDecoder
        |> required "current_page" int
        |> required "last_page" int
        |> required "per_page" int


nextPageNumber: Model a -> Maybe Int
nextPageNumber model =
    if model.current_page == model.last_page then
        Nothing
    else
        Just (model.current_page + 1)


previousPageNumber: Model a -> Maybe Int
previousPageNumber model =
    if model.current_page == 1 then
        Nothing
    else
        Just (model.current_page - 1)
