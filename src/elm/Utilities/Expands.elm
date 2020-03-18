module Utilities.Expands exposing (..)

import Url.Builder as Builder exposing (QueryParameter)


type Expand = Expand String


expand : String -> Expand
expand relation =
    Expand relation


expands : List String -> List Expand
expands relations =
    List.map expand relations


toQueryParameter : Expand -> QueryParameter
toQueryParameter (Expand relation) =
    Builder.string ("expand[" ++ relation ++ "]") "*"


toQueryParameters : List Expand -> List QueryParameter
toQueryParameters relations =
    List.map toQueryParameter relations
