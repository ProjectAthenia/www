-- Main entry point of the App
module Athenia.Main exposing (..)

import Athenia.Api as Api exposing (Token)
import Athenia.Models.User.User as User
import Athenia.Page as Page
import Athenia.Page.Article.Editor as ArticleEditor
import Athenia.Page.Article.Viewer as ArticleViewer
import Athenia.Page.Blank as Blank
import Athenia.Page.Home as Home
import Athenia.Page.Login as Login
import Athenia.Page.NotFound as NotFound
import Athenia.Page.Profile as Profile
import Athenia.Page.Register as Register
import Athenia.Page.Settings as Settings
import Athenia.Route as Route exposing (Route)
import Athenia.Session as Session exposing (Session)
import Athenia.Viewer as Viewer exposing (Viewer)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Task
import Time
import Url exposing (Url)

type Model
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | Settings Settings.Model
    | Login Login.Model
    | Register Register.Model
    | Profile Int Profile.Model
    | ArticleEditor Int ArticleEditor.Model
    | ArticleViewer Int ArticleViewer.Model


-- MODEL


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromViewer navKey maybeViewer))


view : Model -> Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view (Session.viewer (toSession model)) page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            viewPage Page.Other (\_ -> Ignored) Blank.view

        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view

        Settings settings ->
            viewPage Page.Other GotSettingsMsg (Settings.view settings)

        Home home ->
            viewPage Page.Home GotHomeMsg (Home.view home)

        Login login ->
            viewPage Page.Other GotLoginMsg (Login.view login)

        Register register ->
            viewPage Page.Other GotRegisterMsg (Register.view register)

        Profile userId profile ->
            viewPage (Page.Profile userId) GotProfileMsg (Profile.view profile)

        ArticleViewer articleId article ->
            viewPage Page.Other GotArticleViewerMsg (ArticleViewer.view article)

        ArticleEditor articleId article ->
            viewPage Page.Other GotArticleEditorMsg (ArticleEditor.view article)


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotSettingsMsg Settings.Msg
    | GotLoginMsg Login.Msg
    | GotRegisterMsg Register.Msg
    | GotProfileMsg Profile.Msg
    | GotArticleViewerMsg ArticleViewer.Msg
    | GotArticleEditorMsg ArticleEditor.Msg
    | GotSession Session


toSession : Model -> Session
toSession page =
    case page of
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

        Register register ->
            Register.toSession register

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
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model

        Just Route.Register ->
            Register.init session
                |> updateWith Register GotRegisterMsg model

        Just route ->

            case Session.token session of
                Just token ->
                    changeRouteToAuthenticatedRoute route model session token

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
    case ( msg, model ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
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

        ( GotRegisterMsg subMsg, Register register ) ->
            Register.update subMsg register
                |> updateWith Register GotRegisterMsg model

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
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
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

        Register register ->
            Sub.map GotRegisterMsg (Register.subscriptions register)

        Profile _ profile ->
            Sub.map GotProfileMsg (Profile.subscriptions profile)

        ArticleViewer _ article ->
            Sub.map GotArticleViewerMsg (ArticleViewer.subscriptions article)

        ArticleEditor _ editor ->
            Sub.map GotArticleEditorMsg (ArticleEditor.subscriptions editor)


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
