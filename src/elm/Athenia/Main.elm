-- Main entry point of the App
module Athenia.Main exposing (..)

import Athenia.Api as Api exposing (Token)
import Athenia.Components.NavBar as AppNavBar
import Athenia.Models.User.User as User
import Athenia.Page as Page
import Athenia.Page.Article.Editor as ArticleEditor
import Athenia.Page.Article.Viewer as ArticleViewer
import Athenia.Page.Blank as Blank
import Athenia.Page.Home as Home
import Athenia.Page.Login as Login
import Athenia.Page.NotFound as NotFound
import Athenia.Page.Profile as Profile
import Athenia.Page.Settings as Settings
import Athenia.Page.SignUp as SignUp
import Athenia.Route as Route exposing (Route)
import Athenia.Session as Session exposing (Session)
import Athenia.Viewer as Viewer exposing (Viewer)
import Bootstrap.Navbar as Navbar
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Task
import Time
import Url exposing (Url)

type CurrentState
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | Settings Settings.Model
    | Login Login.Model
    | SignUp SignUp.Model
    | Profile Int Profile.Model
    | ArticleEditor Int ArticleEditor.Model
    | ArticleViewer Int ArticleViewer.Model


type alias Model =
    { navBarState : Navbar.State
    , navBarConfig : Navbar.Config Msg
    , currentState : CurrentState
    }


-- MODEL


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    let
        (navBarState, navBarCmd) =
            Navbar.initialState NavBarStateChange
        initialModel =
            { navBarState = navBarState
            , navBarConfig = AppNavBar.config NavBarStateChange (Route.href Route.Home)
                (getNavItems (Viewer.maybeToken maybeViewer))
            , currentState = (Redirect (Session.fromViewer navKey maybeViewer))
            }
        (readyModel, initialCmd) =
            changeRouteTo (Route.fromUrl url)
                initialModel
    in
        ( readyModel
        , Cmd.batch
            [ initialCmd
            , navBarCmd
            ]
        )


getNavItems : Maybe token -> List (AppNavBar.NavLink Msg)
getNavItems maybeToken =
    case maybeToken of
        Just token ->
            [ ("Settings", Route.href Route.Settings)
            , ("Log Out", Route.href Route.Logout)
            ]
        Nothing ->
            [ ("Sign In", Route.href  Route.Login)
            , ("Sign Up", Route.href  Route.SignUp)
            ]



view : Model -> Document Msg
view model =
    let
        viewPage viewData parentMsg =
            Page.view model.navBarState model.navBarConfig viewData parentMsg
    in
    case model.currentState of
        Redirect _ ->
            viewPage Blank.view (\_ -> Ignored)

        NotFound _ ->
            viewPage NotFound.view (\_ -> Ignored)

        Settings settings ->
            viewPage (Settings.view settings) GotSettingsMsg

        Home home ->
            viewPage (Home.view home) GotHomeMsg

        Login login ->
            viewPage (Login.view login) GotLoginMsg

        SignUp signUp ->
            viewPage (SignUp.view signUp) GotSignUpMsg

        Profile userId profile ->
            viewPage (Profile.view profile) GotProfileMsg

        ArticleViewer articleId article ->
            viewPage (ArticleViewer.view article) GotArticleViewerMsg

        ArticleEditor articleId article ->
            viewPage (ArticleEditor.view article) GotArticleEditorMsg


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotSettingsMsg Settings.Msg
    | GotLoginMsg Login.Msg
    | GotProfileMsg Profile.Msg
    | GotSignUpMsg SignUp.Msg
    | GotArticleViewerMsg ArticleViewer.Msg
    | GotArticleEditorMsg ArticleEditor.Msg
    | NavBarStateChange Navbar.State
    | GotSession Session


toSession : Model -> Session
toSession page =
    case page.currentState of
        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        Settings settings ->
            Settings.toSession settings

        Login login ->
            Login.toSession login

        SignUp signUp ->
            SignUp.toSession signUp

        Profile _ profile ->
            Profile.toSession profile

        ArticleViewer _ article ->
            ArticleViewer.toSession article

        ArticleEditor _ editor ->
            ArticleEditor.toSession editor


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( { model | currentState = NotFound session }
            , Cmd.none
            )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model

        Just Route.SignUp ->
            SignUp.init session
                |> updateWith SignUp GotSignUpMsg model

        Just route ->
            case Session.token session of
                Just token ->
                    changeRouteToAuthenticatedRoute route {model | navBarConfig = AppNavBar.updateItems (getNavItems (Just token)) model.navBarConfig} session token

                Nothing ->
                    (model, Route.replaceUrl (Session.navKey session) Route.Login )


changeRouteToAuthenticatedRoute : Route -> Model -> Session -> Token -> (Model, Cmd Msg)
changeRouteToAuthenticatedRoute route model session token =
    case route of
        Route.EditArticle articleId ->
            ArticleEditor.init session token articleId
                |> updateWith (ArticleEditor articleId) GotArticleEditorMsg model

        Route.Settings ->
            Settings.init session token
                |> updateWith Settings GotSettingsMsg model

        Route.Home ->
            Home.init session token
                |> updateWith Home GotHomeMsg model

        Route.Profile userId ->
            Profile.init session token userId
                |> updateWith (Profile userId) GotProfileMsg model

        Route.Article articleId ->
            ArticleViewer.init session token articleId
                |> updateWith (ArticleViewer articleId) GotArticleViewerMsg model

        _ ->
            (model, Cmd.none)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.currentState ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( NavBarStateChange navBarState, _ ) ->
            ( { model | navBarState = navBarState }
            , Cmd.none
            )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotSettingsMsg subMsg, Settings settings ) ->
            Settings.update subMsg settings
                |> updateWith Settings GotSettingsMsg model

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg model

        ( GotSignUpMsg subMsg, SignUp signUp ) ->
            SignUp.update subMsg signUp
                |> updateWith SignUp GotSignUpMsg model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotProfileMsg subMsg, Profile username profile ) ->
            Profile.update subMsg profile
                |> updateWith (Profile username) GotProfileMsg model

        ( GotArticleViewerMsg subMsg, ArticleViewer articleId article ) ->
            ArticleViewer.update subMsg article
                |> updateWith (ArticleViewer articleId) GotArticleViewerMsg model

        ( GotArticleEditorMsg subMsg, ArticleEditor articleId editor ) ->
            ArticleEditor.update subMsg editor
                |> updateWith (ArticleEditor articleId) GotArticleEditorMsg model

        ( GotSession session, Redirect _ ) ->
            ( { model | currentState = Redirect session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> CurrentState) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( {model | currentState = toModel subModel }
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.currentState of
            NotFound _ ->
                Sub.none

            Redirect _ ->
                Session.changes GotSession (Session.navKey (toSession model))

            Settings settings ->
                Sub.map GotSettingsMsg (Settings.subscriptions settings)

            Home home ->
                Sub.map GotHomeMsg (Home.subscriptions home)

            Login login ->
                Sub.map GotLoginMsg (Login.subscriptions login)

            SignUp signUp ->
                Sub.map GotSignUpMsg (SignUp.subscriptions signUp)

            Profile _ profile ->
                Sub.map GotProfileMsg (Profile.subscriptions profile)

            ArticleViewer _ article ->
                Sub.map GotArticleViewerMsg (ArticleViewer.subscriptions article)

            ArticleEditor _ editor ->
                Sub.map GotArticleEditorMsg (ArticleEditor.subscriptions editor)
        , Navbar.subscriptions model.navBarState NavBarStateChange
        ]


main : Program Value Model Msg
main =
    Api.application Viewer.decoder
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
