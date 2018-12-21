module Athenia.Page.Login exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The login page.
-}

import Athenia.Api as Api exposing (Token)
import Athenia.Models.User.User as User
import Athenia.Route as Route exposing (Route)
import Athenia.Session as Session exposing (Session)
import Athenia.Viewer as Viewer exposing (Viewer)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Time



-- MODEL


type alias Model =
    { session : Session
    , currentTime : Time.Posix
    , problems : List Problem
    , form : Form
    }


{-| Recording validation problems on a per-field basis facilitates displaying
them inline next to the field where the error occurred.

I implemented it this way out of habit, then realized the spec called for
displaying all the errors at the top. I thought about simplifying it, but then
figured it'd be useful to show how I would normally model this data - assuming
the intended UX was to render errors per field.

(The other part of this is having a view function like this:

viewFieldErrors : ValidatedField -> List Problem -> Html msg

...and it filters the list of problems to render only InvalidEntry ones for the
given ValidatedField. That way you can call this:

viewFieldErrors Email problems

...next to the `email` field, and call `viewFieldErrors Password problems`
next to the `password` field, and so on.

The `LoginError` should be displayed elsewhere, since it doesn't correspond to
a particular field.

-}
type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


init : Time.Posix -> Session -> ( Model, Cmd msg )
init currentTime session =
    ( { session = session
      , currentTime = currentTime
      , problems = []
      , form =
        { email = ""
        , password = ""
        }
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        div [ id "login", class "page center-content" ]
            [ Grid.container []
                [ Grid.row []
                    [ Grid.col [Col.md6, Col.offsetMd3]
                        [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
                        , p [ class "text-xs-center" ]
                            [ a [ Route.href Route.SignUp ]
                                [ text "Need an account?" ]
                            ]
                        , ul [ class "error-messages" ]
                            (List.map viewProblem model.problems)
                        , viewForm model.form
                        ]
                    ]
                ]
            ]
    }


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                InvalidEntry _ str ->
                    str

                ServerError str ->
                    str
    in
    li [] [ text errorMessage ]


viewForm : Form -> Html Msg
viewForm form =
    Form.form [ onSubmit SubmittedForm ]
        [ fieldset [ class "form-group" ]
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
            ] [ text "Sign In" ]
        ]



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | CompletedLogin (Result Http.Error Api.Token)
    | RetrieveMe Token (Result Http.Error User.Model)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Http.send CompletedLogin (login validForm)
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedLogin (Err error) ->
            handleErrors error model

        CompletedLogin (Ok token) ->
            ( model
            , Http.send (RetrieveMe token) (getMe token)
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        RetrieveMe token (Err error) ->
            handleErrors error model

        RetrieveMe token (Ok user) ->
            let
                viewer =
                    Viewer.viewer user token (Time.posixToMillis model.currentTime)
            in
            ( {model
                | session =
                    Session.fromViewer (Session.navKey model.session) (Just viewer)
            }
            , Cmd.batch
                [ Viewer.store viewer
                , Route.replaceUrl (Session.navKey model.session) Route.Home
                ]
            )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )


handleErrors : Http.Error -> Model -> (Model, Cmd Msg)
handleErrors errors model =
    let
        serverErrors =
            Api.decodeErrors errors
                |> List.map ServerError
    in
    ( { model | problems = List.append model.problems serverErrors }
    , Cmd.none
    )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- FORM

type alias Form =
    { email : String
    , password : String
    }

{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!
-}
type ValidatedField
    = Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
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
            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]

                else
                    []

trimFields : Form -> TrimmedForm
trimFields user =
    Trimmed
        { email = (String.trim user.email)
        , password = (String.trim user.password)
        }



-- HTTP


login : TrimmedForm -> Http.Request Token
login (Trimmed model) =
    let
        user =
            { id = 0
            , email = model.email
            , password = model.password
            , name = ""
            , roles = []
            }
        body =
            Http.jsonBody (User.toJson user)
    in
        Api.login body


getMe : Token -> Http.Request User.Model
getMe token =
    Api.me token


-- EXPORT


toSession : Model -> Session
toSession model =
    model.session