module Components.CRUD.SharedConfiguration exposing (..)

import Api.Group exposing (..)
import Json.Decode exposing (..)
import Utilities.Expands as Expands
import Utilities.ModelHelpers exposing (..)


-- This configuration model is generated from the root controller using configuration variables used for both the form and list
type alias Configuration dataModel =
    { apiUrl: String
    , resourceName: String
    , pageUrl: String
    , routeGroup: RouteGroup
    , decoder: Decoder (GenericModel dataModel)
    , expands: List Expands.Expand
    }


configure: String -> String -> String -> RouteGroup -> Decoder (GenericModel dataModel) -> List Expands.Expand -> Configuration dataModel
configure apiUrl resourceName pageUrl routeGroup decoder expands =
    { apiUrl = apiUrl
    , resourceName = resourceName
    , pageUrl = pageUrl
    , routeGroup = routeGroup
    , decoder = decoder
    , expands = expands
    }
