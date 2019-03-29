module Page exposing (Page(..), view, viewErrors)

import Bootstrap.CDN as CDN
import Bootstrap.Navbar as Navbar
import Browser exposing (Document)
import Html exposing (Html, a, button, div, footer, p, span, text)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Html.Parser as Parser
import Html.Parser.Util as ParserUtil
import Utilities.Configuration as Configuration


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type Page
    = Other
    | Home
    | Login
    | Profile Int
    | Article


{-| Take a page's Html and frames it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
view : Configuration.Model -> Navbar.State -> Navbar.Config msg -> { title : String, content : Html subMsg } -> (subMsg -> msg) -> Document msg
view configuration navBarState navBarConfig { title, content } parentMsg =
    { title = title ++ " - " ++ Configuration.getAppName configuration
    , body = CDN.stylesheet :: viewHeader navBarState navBarConfig :: Html.map parentMsg content :: [ viewFooter configuration ]
    }


viewHeader : Navbar.State -> Navbar.Config msg ->  Html msg
viewHeader navBarState navBarConfig =
    Navbar.view navBarState navBarConfig


viewFooter : Configuration.Model -> Html msg
viewFooter configuration =
    footer []
        [ div [ class "container" ]
            [ a [ class "logo-font", href "/" ]
                [ text <| Configuration.getAppName configuration ]
            , span [ class "attribution" ]
                <| case Configuration.fetchConfigVariable configuration "FOOTER_MESSAGE" of
                    Just footerMessage ->
                        case Parser.run footerMessage of
                            Ok result ->
                                ParserUtil.toVirtualDom result
                            Err _ ->
                                [ text footerMessage ]

                    Nothing ->
                        [ text "A governing system built for the 21st century by "
                        , a [ href "https://pomeloproductions.com" ] [ text "Pomelo Productions" ]
                        , text ". Code & design licensed under MIT."
                        ]
            ]
        ]


{-| Render dismissable errors. We use this all over the place!
-}
viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.map (\error -> p [] [ text error ]) errors
                ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]