-- Main entry point of the App
module Main exposing (..)

import Api as Api exposing (Token)
import Bootstrap.Navbar as Navbar
import Browser exposing (Document)
import Browser.Navigation as Nav
import Components.NavBar as AppNavBar
import Dict
import Json.Decode as Decode exposing (Value)
import Json.Decode.Pipeline as Pipeline
import Models.User.User as User
import Page as Page
import Page.Article.Editor as ArticleEditor
import Page.Article.Viewer as ArticleViewer
import Page.Blank as Blank
import Page.ConfigError as ConfigError
import Page.Home as Home
import Page.Loading as Loading
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Settings as Settings
import Page.SignUp as SignUp
import Route as Route exposing (Route)
import Session as Session exposing (Session)
import Task
import Time
import Url exposing (Url)
import Utilities.Configuration as Configuration
import Viewer as Viewer exposing (Viewer)

type CurrentState
    = Redirect Session
    | NotFound Session
    | ConfigError String Session
    | Loading Url Session
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
    , configuration: Configuration.Model
    , apiUrl: String
    , currentState : CurrentState
    , currentTime : Time.Posix
    , refreshingToken : Bool
    }


type alias Flags =
    { maybeViewer: Maybe Viewer
    , config: Configuration.Model
    }


-- MODEL


defaultFlags: Flags
defaultFlags =
    { maybeViewer = Nothing
    , config = Dict.fromList []
    }


appInit : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
appInit flags url navKey =
    let
        (navBarState, navBarCmd) =
            Navbar.initialState NavBarStateChange
        maybeToken =
            Viewer.maybeToken flags.maybeViewer
        initialModel =
            { navBarState = navBarState
            , navBarConfig = AppNavBar.config flags.config NavBarStateChange (Route.href Route.Home)
                (getNavItems maybeToken)
            , currentState
                = case maybeToken of
                    Just _ ->
                        (Loading url (Session.fromViewer navKey flags.maybeViewer))
                    Nothing ->
                        (Redirect (Session.fromViewer navKey flags.maybeViewer))

            , currentTime = Time.millisToPosix 0
            , configuration = flags.config
            , apiUrl
                = case Configuration.fetchConfigVariable flags.config "API_URL" of
                    Just apiUrl ->
                        apiUrl
                    Nothing ->
                        ""
            , refreshingToken = False
            }
        (readyModel, initialCmd) =
            case maybeToken of
                Just _ ->
                    (initialModel, Cmd.none)
                Nothing ->
                    changeRouteTo (Route.fromUrl url)
                        initialModel
    in
        ( { readyModel
            | currentState
                = if String.length readyModel.apiUrl == 0 then
                    ConfigError "Please set the API url in the .env file" (Session.fromViewer navKey flags.maybeViewer)
                else
                    readyModel.currentState
        }
        , Cmd.batch
            [ initialCmd
            , navBarCmd
            , Task.perform Tick Time.now
            ]
        )


getNavItems : Maybe token -> List (AppNavBar.NavLink Msg)
getNavItems maybeToken =
    case maybeToken of
        Just _ ->
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
            Page.view model.configuration model.navBarState model.navBarConfig viewData parentMsg
    in
    case model.currentState of
        Redirect _ ->
            viewPage Blank.view (\_ -> Ignored)

        Loading _ _ ->
            viewPage Loading.view (\_ -> Ignored)

        NotFound _ ->
            viewPage NotFound.view (\_ -> Ignored)

        ConfigError message _ ->
            viewPage (ConfigError.view message) (\_ -> Ignored)

        Settings settings ->
            viewPage (Settings.view settings) GotSettingsMsg

        Home home ->
            viewPage (Home.view home) GotHomeMsg

        Login login ->
            viewPage (Login.view login) GotLoginMsg

        SignUp signUp ->
            viewPage (SignUp.view signUp) GotSignUpMsg

        Profile _ profile ->
            viewPage (Profile.view profile) GotProfileMsg

        ArticleViewer _ article ->
            viewPage (ArticleViewer.view article) GotArticleViewerMsg

        ArticleEditor _ article ->
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
    | CompletedTokenRefresh (Result Api.Error Api.Token)
    | Tick Time.Posix


toSession : Model -> Session
toSession page =
    case page.currentState of
        Loading _ session ->
            session

        Redirect session ->
            session

        NotFound session ->
            session

        ConfigError _ session ->
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
            ( {model
                | navBarConfig = AppNavBar.updateItems (getNavItems Nothing) model.navBarConfig
            }, Cmd.batch
                [ Api.logout
                , Route.replaceUrl (Session.navKey session) Route.Login
                ]
            )

        Just Route.Login ->
            Login.init model.currentTime session model.apiUrl
                |> updateWith Login GotLoginMsg model

        Just Route.SignUp ->
            SignUp.init model.currentTime session model.apiUrl
                |> updateWith SignUp GotSignUpMsg model

        Just route ->
            case (Session.token session, Session.user session) of
                (Just token, Just user) ->
                    changeRouteToAuthenticatedRoute route {model | navBarConfig = AppNavBar.updateItems (getNavItems (Just token)) model.navBarConfig} session token user

                _ ->
                    (model, Route.replaceUrl (Session.navKey session) Route.Login )


changeRouteToAuthenticatedRoute : Route -> Model -> Session -> Token -> User.Model -> (Model, Cmd Msg)
changeRouteToAuthenticatedRoute route model session token user =
    case route of
        Route.EditArticle articleId ->
            ArticleEditor.init session model.apiUrl token articleId
                |> updateWith (ArticleEditor articleId) GotArticleEditorMsg model

        Route.Settings ->
            Settings.init session model.apiUrl token
                |> updateWith Settings GotSettingsMsg model

        Route.Home ->
            Home.init session model.apiUrl token user
                |> updateWith Home GotHomeMsg model

        Route.Profile userId ->
            Profile.init session model.apiUrl token userId
                |> updateWith (Profile userId) GotProfileMsg model

        Route.Article articleId ->
            ArticleViewer.init session model.apiUrl token articleId
                |> updateWith (ArticleViewer articleId) GotArticleViewerMsg model

        _ ->
            (model, Cmd.none)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.currentState ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( Tick currentTime, pageState ) ->
            let
                needsRefresh =
                    Session.needsAuthRefresh currentTime
                        <| toSession model

                (updatedModel, cmd) =
                    case pageState of
                        Login loginModel ->
                            ( { model | currentState = Login {loginModel | currentTime = currentTime } }
                            , Cmd.none
                            )

                        SignUp signUpModel ->
                            ( { model | currentState = SignUp {signUpModel | currentTime = currentTime } }
                            , Cmd.none
                            )

                        Loading url _ ->
                            if needsRefresh == False then
                                changeRouteTo (Route.fromUrl url) model
                            else
                                (model, Cmd.none)

                        _ ->
                            (model, Cmd.none)

                session =
                    (toSession model)

                maybeToken =
                    Viewer.maybeToken
                        <| Session.viewer session
            in
            ( { updatedModel
                | currentTime = currentTime
                , refreshingToken = needsRefresh
            }
            , Cmd.batch
                [ cmd
                , case (needsRefresh, maybeToken) of
                    (True, Just token) ->
                        (Api.refresh model.apiUrl token CompletedTokenRefresh)
                    (True, Nothing) ->
                        Route.replaceUrl (Session.navKey session) Route.Login
                    _ ->
                        Cmd.none

                ]
            )

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

        ( CompletedTokenRefresh (Ok token), _ ) ->
            case Session.user (toSession model) of
                Just user ->
                    let
                        viewer =
                            Viewer.viewer user token (Time.posixToMillis model.currentTime)
                        (updatedModel, cmd) =
                            case model.currentState of
                                Loading url session ->
                                    changeRouteTo (Route.fromUrl url)
                                        {model | currentState = Loading url (Session.fromViewer (Session.navKey session) (Just viewer))}
                                _ ->
                                    (model, Cmd.none)
                    in
                    ( { updatedModel
                        | refreshingToken = False
                    }
                    , Cmd.batch
                        [ cmd
                        , Viewer.store viewer
                        ]
                    )
                Nothing ->
                    changeRouteTo (Just Route.Login) model

        ( CompletedTokenRefresh (Err _), _ ) ->
            changeRouteTo (Just Route.Logout) model

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
            Loading _ _ ->
                Sub.none

            NotFound _ ->
                Sub.none

            ConfigError _ _ ->
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
        , Time.every 1000 Tick
        ]


flagsDecoder: Decode.Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> Pipeline.optional "maybeViewer" (Decode.maybe (Api.storageDecoder Viewer.decoder)) Nothing
        |> Pipeline.optional "config" Configuration.decoder (Dict.fromList [])


application :
        { init : Flags -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application config =
    let
        init flags url navKey =
            let
                dummy =
                    Debug.log "init" (Decode.decodeValue Decode.string flags)
                decodedFlags =
                    case Decode.decodeValue flagsDecoder flags of
                        Ok result ->
                            result
                        _ ->
                            defaultFlags
            in
            config.init decodedFlags url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


main : Program Value Model Msg
main =
    application
        { init = appInit
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
