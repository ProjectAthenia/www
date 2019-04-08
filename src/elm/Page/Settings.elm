module Page.Settings exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api as Api exposing (Token)
import Api.Endpoint as Endpoint
import Components.LoadingIndicator as LoadingIndicator
import Models.Error as Error
import Models.MembershipPlan.MembershipPlan as MembershipPlan
import Models.Payment.PaymentMethod as PaymentMethod
import Models.User.User as User
import Route as Route
import Session as Session exposing (Session)
import Utilities.Log as Log
import Viewer as Viewer exposing (Viewer)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (Html, div, fieldset, h1, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onCheck)
import Http
import Json.Encode as Encode
import Ports.Stripe as Stripe



-- MODEL


type alias Model =
    { session : Session
    , showLoading : Bool
    , apiUrl : String
    , token : Token
    , problems : List Problem
    , status : Status
    , maybeUser : Maybe User.Model
    , membershipPlans : List MembershipPlan.Model
    , selectedMembershipPlan : Maybe MembershipPlan.Model
    , selectedPaymentMethod : (Bool, Maybe PaymentMethod.Model)
    }


type alias Form =
    { id : Int
    , name : String
    , email : String
    , password : String
    }


type Status
    = Loading
    | Loaded Form
    | Failed


type Problem
    = InvalidEntry ValidatedField String
    | ServerError Error.Model


init : Session -> String -> Token -> ( Model, Cmd Msg )
init session apiUrl token =
    ( { session = session
      , showLoading = True
      , apiUrl = apiUrl
      , token = token
      , problems = []
      , status = Loading
      , maybeUser = Nothing
      , membershipPlans = []
      , selectedMembershipPlan = Nothing
      , selectedPaymentMethod = (False, Nothing)
      }
    , Cmd.batch
        [ Api.get (Endpoint.me apiUrl) (Session.token session) User.modelDecoder CompletedFormLoad
        ]
    )


{-| A form that has been validated. Only the `edit` function uses this. Its
purpose is to prevent us from forgetting to validate the form before passing
it to `edit`.

This doesn't create any guarantees that the form was actually validated. If
we wanted to do that, we'd need to move the form data into a separate module!

-}
type ValidForm
    = Valid Form



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Settings"
    , content =
        div [ id "settings", class "page" ]
            [ Grid.container []
                [ Grid.row []
                    [ Grid.col [Col.md6, Col.offsetMd3]
                        [ h1 [ class "text-xs-center" ] [ text "Your Settings" ]
                        , div [ ]
                            (List.map viewProblem model.problems)
                        , case model.status of
                            Loaded settingsForm ->
                                div []
                                    [ case (List.length model.membershipPlans > 0, model.maybeUser) of
                                        (True, Just user) ->
                                            viewSubscriptionForm model user
                                        _ ->
                                            text ""
                                    , viewSettingsForm model.token settingsForm
                                    ]

                            Loading ->
                                text ""

                            Failed ->
                                text "Error loading page."
                        ]
                    ]
                ]
            , LoadingIndicator.view model.showLoading
            ]
    }


viewSubscriptionForm : Model -> User.Model -> Html Msg
viewSubscriptionForm model user =
    Form.form [ onSubmit (SubmittedSubscriptionForm) ]
        [ div []
            <| List.map (viewMembershipPlanOption model.selectedMembershipPlan) model.membershipPlans
        , if List.length user.payment_methods == 0 then
            viewStripeForm
          else
            div []
                <| List.concat
                    [ List.map (viewPaymentMethod model.selectedPaymentMethod) user.payment_methods
                    , [viewStripeOption model.selectedPaymentMethod]
                    ]
        , Button.button
            [ Button.primary
            , Button.large
            ] [ text "Submit Payment" ]
        ]


viewMembershipPlanOption : Maybe MembershipPlan.Model -> MembershipPlan.Model -> Html Msg
viewMembershipPlanOption selectedMembershipPlan membershipPlan =
    Button.radioButton
        (isMembershipPlanSelected membershipPlan selectedMembershipPlan)
        [ Button.attrs [onCheck (SelectedMembershipPlan membershipPlan)]
        ] [ text (MembershipPlan.makeReadable membershipPlan) ]


viewStripeForm : Html Msg
viewStripeForm =
    div [ id "card-element" ] []


viewPaymentMethod : (Bool, Maybe PaymentMethod.Model) -> PaymentMethod.Model -> Html Msg
viewPaymentMethod maybeSelectedMembershipPlan paymentMethod =
    Button.radioButton
        (isPaymentMethodSelected paymentMethod maybeSelectedMembershipPlan)
        [ Button.attrs [onCheck (SelectedPaymentMethod (Just paymentMethod))]
        ] [ text (PaymentMethod.makeReadable paymentMethod) ]


viewStripeOption : (Bool, Maybe PaymentMethod.Model) -> Html Msg
viewStripeOption maybeSelectedMembershipPlan =
    let
        optionSelected = maybeSelectedMembershipPlan == (True, Nothing)
    in
    div []
        [ Button.radioButton optionSelected
            [ Button.attrs [onCheck (SelectedPaymentMethod Nothing)]
            ] [ text "New Credit Card" ]
        , div [ style "display" (if optionSelected then "block" else "none")] [viewStripeForm]
        ]



viewSettingsForm : Token -> Form -> Html Msg
viewSettingsForm token form =
    Form.form [ onSubmit (SubmittedSettingsForm token form) ]
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
                [ Input.placeholder "New Password"
                , Input.onInput EnteredPassword
                , Input.value form.password
                ]
            ]
        , Button.button
            [ Button.primary
            , Button.large
            ] [ text "Update Settings" ]
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
    = SubmittedSettingsForm Token Form
    | SubmittedSubscriptionForm
    | SelectedMembershipPlan MembershipPlan.Model Bool
    | SelectedPaymentMethod (Maybe PaymentMethod.Model) Bool
    | EnteredName String
    | EnteredEmail String
    | EnteredPassword String
    | CompletedFormLoad (Result Api.Error User.Model)
    | CompletedSave (Result Api.Error User.Model)
    | CreatedPaymentMethod (Result Api.Error PaymentMethod.Model)
    | GotSession Session
    | TokenCreated String
    | StripeError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedFormLoad (Ok user) ->
            let
                settingsForm =
                    { id = user.id
                    , name = user.name
                    , email = user.email
                    , password = ""
                    }
            in
            ( { model
                | showLoading = False
                , status = Loaded settingsForm
                , maybeUser = Just user
            }
            , Stripe.initStripeForm "card-element"
            )

        CompletedFormLoad (Err _) ->
            ( { model | showLoading = True, status = Failed }
            , Cmd.none
            )

        SubmittedSettingsForm token form ->
            case validate form of
                Ok validForm ->
                    ( { model | showLoading = True }
                    , edit model.apiUrl token validForm
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        SubmittedSubscriptionForm ->
            ( { model
                | showLoading = True
            }
            , Stripe.createPaymentToken "card-element"
            )

        TokenCreated stripeToken ->
            ( model
            , case model.maybeUser of
                Just user ->
                    createPaymentMethod model.apiUrl model.token user.id stripeToken
                Nothing ->
                    Cmd.none
            )

        SelectedMembershipPlan membershipPlan selected ->
            ({ model
                | selectedMembershipPlan
                    = if selected then Just membershipPlan else Nothing
            }
            , Cmd.none
            )

        SelectedPaymentMethod maybePaymentMethod selected ->
            ({ model
                | selectedPaymentMethod
                    = if selected then (True, maybePaymentMethod) else (True, Nothing)
            }
            , Cmd.none
            )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredName name ->
            updateForm (\form -> { form | name = name }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedSave (Err error) ->
            case error of
                Api.BadStatus _ errorModel ->
                    ( { model | showLoading = False, problems = List.append model.problems [ServerError errorModel] }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        CompletedSave (Ok user) ->
            ( { model | showLoading = False }
            ,  case (Session.token model.session, Session.lastRefresh model.session) of
                (Just token, Just lastRefresh) ->
                    Viewer.store
                        <| Viewer.viewer user token lastRefresh
                _ ->
                    Cmd.none
            )

        StripeError _ ->
            ( { model | showLoading = False }
            , Cmd.none
            )

        GotSession session ->
            case Viewer.maybeToken (Session.viewer session) of
                Just token ->
                    ( { model
                        | session = session
                        , token = token
                    }
                    , Cmd.none
                    )
                Nothing ->
                    ( model
                    , Route.replaceUrl (Session.navKey session) Route.Login
                    )

        CreatedPaymentMethod (Ok paymentMethod) ->
            ( { model
                | showLoading = False
            }
            , Cmd.none
            )

        CreatedPaymentMethod (Err error) ->
            case error of
                Api.BadStatus _ errorModel ->
                    ( { model | showLoading = False, problems = List.append model.problems [ServerError errorModel] }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )



{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd msg )
updateForm transform model =
    case model.status of
        Loaded form ->
            ( { model | status = Loaded (transform form) }, Cmd.none )

        _ ->
            ( model, Log.error )


isMembershipPlanSelected : MembershipPlan.Model -> Maybe MembershipPlan.Model -> Bool
isMembershipPlanSelected membershipPlan maybeSelectedMembershipPlan =
    case maybeSelectedMembershipPlan of
        Just selectedMembershipPlan ->
            selectedMembershipPlan.id == membershipPlan.id
        Nothing ->
            False


isPaymentMethodSelected : PaymentMethod.Model -> (Bool, Maybe PaymentMethod.Model) -> Bool
isPaymentMethodSelected paymentMethod maybeSelectedPaymentMethod =
    case maybeSelectedPaymentMethod of
        (True, Just selectedPaymentMethod) ->
            selectedPaymentMethod.id == paymentMethod.id
        _ ->
            False



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Session.changes GotSession (Session.navKey model.session)
        , Stripe.tokenCreated TokenCreated
        , Stripe.stripeError StripeError
        ]



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

NOTE: there are no ImageUrl or Bio variants here, because they aren't validated!

-}
type ValidatedField
    = Username
    | Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Username
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
            Username ->
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
                let
                    passwordLength =
                        String.length form.password
                in
                if passwordLength > 0 && passwordLength < Viewer.minPasswordChars then
                    [ "Password must be at least " ++ String.fromInt Viewer.minPasswordChars ++ " characters long." ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { id = form.id
        , name = String.trim form.name
        , email = String.trim form.email
        , password = String.trim form.password
        }



-- HTTP


{-| This takes a Valid Form as a reminder that it needs to have been validated
first.
-}
edit : String -> Token -> TrimmedForm -> Cmd Msg
edit apiUrl token (Trimmed form) =
    let
        encodedUser =
            Encode.object
                <| List.concat
                    [ if String.length form.email > 0 then
                        [ ("email", Encode.string form.email) ]
                    else
                        []
                    , if String.length form.password > 0 then
                        [ ("password", Encode.string form.password) ]
                    else
                        []
                    , if String.length form.name > 0 then
                        [ ("name", Encode.string form.name) ]
                    else
                        []
                    ]

        body =
            Http.jsonBody encodedUser
    in
    Api.settings apiUrl token form.id body CompletedSave


createPaymentMethod : String -> Token -> Int -> String -> Cmd Msg
createPaymentMethod apiUrl token userId stripeToken =
    let
        encodedPaymentMethod =
            Encode.object
                [ ("token", Encode.string stripeToken)
                ]
    in
        Api.createUserPaymentMethod apiUrl token userId (Http.jsonBody encodedPaymentMethod) CreatedPaymentMethod
