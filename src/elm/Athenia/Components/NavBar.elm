module Athenia.Components.NavBar exposing (NavLink, config)

import Bootstrap.Navbar as Navbar
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias NavLink msg =
    (String, Attribute msg)


config : (Navbar.State -> msg) -> Attribute msg -> List (NavLink msg) -> Navbar.Config msg
config msg brand navLinks =
    Navbar.config msg
        |> Navbar.brand [ brand ] [ text "Project Athenia"]
        |> Navbar.items (List.map createNavItem navLinks)


createNavItem : NavLink msg -> Navbar.Item msg
createNavItem (navText, navHref) =
    Navbar.itemLink [navHref] [text navText]
