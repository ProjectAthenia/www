-- Simple module for interacting with app search fields
module Utilities.SearchField exposing (..)

import Url.Builder exposing (QueryParameter, string)


type SearchFieldType
    = Text
    | Number
    | Equals
    | None
    | Select (List (String, String))


type alias Model =
    { name: String
    , value: String
    , type_: SearchFieldType
    }


model : String -> SearchFieldType -> Model
model name type_ =
    { name = name
    , value = ""
    , type_ = type_
    }


setValue: Model -> String -> Model
setValue instance value =
    { instance
        | value = value
    }


buildSearchFieldQueryKey : Model -> String
buildSearchFieldQueryKey instance =
    case instance.type_ of
        Select _ ->
            "filter[" ++ instance.name ++ "]"

        _ ->
            "search[" ++ instance.name ++ "]"


buildSearchFieldQueryValue : Model -> String
buildSearchFieldQueryValue instance =
    case instance.type_ of
        None ->
            ""
        Text ->
            if String.length instance.value == 0 then "" else "like,*" ++ instance.value ++ "*"
        Number ->
            "eq," ++ instance.value
        Equals ->
            "eq," ++ instance.value
        Select _ ->
            if String.length instance.value == 0 then "" else "eq," ++ instance.value


-- Builds a single expand field string
buildSearchFieldQuery : Model -> Maybe QueryParameter
buildSearchFieldQuery instance =
    case String.length instance.value > 0 of
        True ->
            Just
                <| string (buildSearchFieldQueryKey instance) (buildSearchFieldQueryValue instance)
        False ->
            Nothing


buildSearchQuery : List Model -> List QueryParameter
buildSearchQuery searchFields =
    List.filterMap buildSearchFieldQuery searchFields
