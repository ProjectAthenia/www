module Athenia.Page.SignUp exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Athenia.Api as Api exposing (Token)
import Athenia.Components.LoadingIndicator as LoadingIndicator
import Athenia.Models.Error as Error
import Athenia.Models.User.User as User
import Athenia.Route as Route exposing (Route)
import Athenia.Session as Session exposing (Session)
import Athenia.Viewer as Viewer exposing (Viewer)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Time


-- MODEL


type alias Model =
    { session : Session
    , showLoading : Bool
    , currentTime : Time.Posix
    , problems : List Problem
    , form : Form
    }


type alias Form =
    { name : String
    , email : String
    , password : String
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError Error.Model


init : Time.Posix -> Session -> ( Model, Cmd msg )
init currentTime session =
    ( { session = session
      , showLoading = False
      , currentTime = currentTime
      , problems = []
      , form =
            { name = ""
            , email = ""
            , password = ""
            }
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register"
    , content =
        div [ id "sign-up", class "page center-content" ]
            [ Grid.container []
                [ Grid.row []
                    [ Grid.col [Col.md6, Col.offsetMd3]
                        [ h1 [ class "text-xs-center" ] [ text "Sign up" ]
                        , p [ class "text-xs-center" ]
                            [ a [ Route.href Route.Login ]
                                [ text "Have an account?" ]
                            ]
                        , div [ ]
                            (List.map viewProblem model.problems)
                        , viewForm model.form
                        ]
                    ]
                ]
            , LoadingIndicator.view model.showLoading
            ]
    }


viewForm : Form -> Html Msg
viewForm form =
    Form.form [ onSubmit SubmittedForm ]
        [ fieldset [ class "form-group" ]
            [ Input.text
                [ Input.large
                , Input.placeholder "Name"
                , Input.onInput EnteredName
                , Input.value form.name
                , Input.attrs [required True]
                ]
            ]
        , fieldset [ class "form-group" ]
            [ Input.email
                [ Input.large
                , Input.placeholder "Email"
                , Input.onInput EnteredEmail
                , Input.value form.email
                , Input.attrs [required True]
                ]
            ]
        , fieldset [ class "form-group" ]
            [ Input.password
                [ Input.placeholder "Password"
                , Input.onInput EnteredPassword
                , Input.value form.password
                , Input.attrs [required True]
                ]
            ]
        , Button.button
            [ Button.primary
            , Button.large
            ] [ text "Sign Up" ]
        ]


viewProblem : Problem -> Html msg
viewProblem problem =
    case problem of
        InvalidEntry _ str ->
            div [class "error"] [text str]

        ServerError error ->
            Error.view error



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredName String
    | EnteredEmail String
    | EnteredPassword String
    | CompletedRegister (Result Api.Error Token)
    | LoadedMe Token (Result Api.Error User.Model)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | showLoading = True, problems = [] }
                    , register validForm
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredName name ->
            updateForm (\form -> { form | name = name }) model

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedRegister (Err error) ->
            case error of
                Api.BadStatus status errorModel ->
                    ( { model | showLoading = False, problems = List.append model.problems [ServerError errorModel] }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        CompletedRegister (Ok token) ->
            ( model
            , loadMe token
            )

        LoadedMe token (Err error) ->
            case error of
                Api.BadStatus status errorModel ->
                    ( { model | showLoading = False, problems = List.append model.problems [ServerError errorModel] }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        LoadedMe token (Ok user) ->
            ( { model | showLoading = False }
            , Viewer.store
                <| Viewer.viewer user token (Time.posixToMillis model.currentTime)
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!
-}
type ValidatedField
    = Name
    | Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Name
    , Email
    , Password
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Name ->
                if String.isEmpty form.name then
                    [ "Name can't be blank." ]

                else
                    []

            Email ->
                if String.isEmpty form.email then
                    [ "Email can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]

                else if String.length form.password < Viewer.minPasswordChars then
                    [ "password must be at least " ++ String.fromInt Viewer.minPasswordChars ++ " characters long." ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { name = String.trim form.name
        , email = String.trim form.email
        , password = String.trim form.password
        }



-- HTTP


register : TrimmedForm -> Cmd Msg
register (Trimmed form) =
    let
        body =
            Encode.object
                [ ( "name", Encode.string form.name )
                , ( "email", Encode.string form.email )
                , ( "password", Encode.string form.password )
                ]
                |> Http.jsonBody
    in
    Api.signUp body CompletedRegister


loadMe : Token -> Cmd Msg
loadMe token =
    Api.me token (LoadedMe token)