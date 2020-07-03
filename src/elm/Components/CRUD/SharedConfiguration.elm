module Components.CRUD.SharedConfiguration exposing (..)

import Api.Group exposing (..)
import Browser.Navigation as Navigation
import Json.Decode exposing (..)
import Utilities.Expands as Expands
import Utilities.ModelHelpers exposing (..)


-- This configuration model is generated from the root controller using configuration variables used for both the form and list
type alias Configuration dataModel =
    { resourceName: String
    , apiUrl: String
    , pageUrl: String
    , routeGroup: RouteGroup
    , decoder: Decoder (GenericModel dataModel)
    , expands: List Expands.Expand
    }


configure: String -> String -> String -> RouteGroup -> Decoder (GenericModel dataModel) -> List Expands.Expand -> Configuration dataModel
configure resourceName apiUrl pageUrl routeGroup decoder expands =
    { resourceName = resourceName
    , apiUrl = apiUrl
    , pageUrl = pageUrl
    , routeGroup = routeGroup
    , decoder = decoder
    , expands = expands
    }
