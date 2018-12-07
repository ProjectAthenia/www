module Athenia.Route exposing
    ( Route(..)
    , fromUrl
    , href
    , replaceUrl
    )

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, int, string)
import Athenia.Models.User.User as User


-- ROUTING


type Route
    = Home
    | Root
    | Login
    | Logout
    | Register
    | Settings
    | Article Int
    | Profile Int
    | NewArticle
    | EditArticle Int


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map Settings (s "settings")
        , Parser.map Profile (s "profile" </> int)
        , Parser.map Register (s "register")
        , Parser.map Article (s "article" </> int)
        , Parser.map NewArticle (s "editor")
        , Parser.map EditArticle (s "editor" </> int)
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Root ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Register ->
                    [ "register" ]

                Settings ->
                    [ "settings" ]

                Article articleId ->
                    [ "article", String.fromInt articleId ]

                Profile userId ->
                    [ "profile", String.fromInt userId ]

                NewArticle ->
                    [ "editor" ]

                EditArticle articleId ->
                    [ "editor", String.fromInt articleId ]
    in
    "#/" ++ String.join "/" pieces