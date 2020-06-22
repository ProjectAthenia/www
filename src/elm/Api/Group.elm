module Api.Group exposing (..)

import Api.Endpoint as Endpoint exposing (..)
import Url.Builder exposing (QueryParameter)


type alias ExistingEndpoint
    = List QueryParameter -> Int -> Endpoint

type alias NewEndpoint
    = Endpoint

type alias IndexEndpoint
    = List QueryParameter -> Endpoint

type alias RouteGroup =
    { existing: ExistingEndpoint
    , new: NewEndpoint
    , index: IndexEndpoint
    }


baseGroup: String -> String -> RouteGroup
baseGroup path apiUrl =
    { existing = existing path apiUrl
    , new = new path apiUrl
    , index = index path apiUrl
    }

existing : String -> String -> List QueryParameter -> Int -> Endpoint
existing path apiUrl queryParams id =
    Endpoint.url apiUrl [ path, String.fromInt id ] queryParams


new : String -> String -> Endpoint
new path apiUrl =
    index path apiUrl []

index : String -> String -> List QueryParameter -> Endpoint
index path apiUrl queryParams =
    Endpoint.url apiUrl [ path ] queryParams