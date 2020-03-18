module Utilities.Order exposing (..)

import Url.Builder as Builder exposing (QueryParameter)


type Direction
    = ASC
    | DESC


type Order = Order Direction String


orderAsc: String -> Order
orderAsc =
    Order ASC


orderDesc: String -> Order
orderDesc =
    Order DESC


directionToString: Direction -> String
directionToString direction =
    case direction of
        ASC ->
            "asc"
        DESC ->
            "desc"


toQueryParameter : Order -> QueryParameter
toQueryParameter (Order direction field) =
    Builder.string ("order[" ++ field ++ "]")
        <| directionToString direction


toQueryParameters : List Order -> List QueryParameter
toQueryParameters orders =
    List.map toQueryParameter orders
