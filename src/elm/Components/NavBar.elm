module Components.NavBar exposing (NavLink, config, updateItems)

import Bootstrap.Navbar as Navbar
import Html exposing (..)


type alias NavLink msg =
    (String, Attribute msg)


config : (Navbar.State -> msg) -> Attribute msg -> List (NavLink msg) -> Navbar.Config msg
config msg brand navLinks =
    Navbar.config msg
        |> Navbar.brand [ brand ] [ text "Project Athenia"]
        |> Navbar.items (List.map createNavItem navLinks)


updateItems : List (NavLink msg) -> Navbar.Config msg -> Navbar.Config msg
updateItems navLinks navBarConfig =
    Navbar.items (List.map createNavItem navLinks) navBarConfig


createNavItem : NavLink msg -> Navbar.Item msg
createNavItem (navText, navHref) =
    Navbar.itemLink [navHref] [text navText]
