module Page.Admin.MembershipPlans.Form exposing (..)

import Api exposing (Error, Token)
import Api.Endpoints.Radical as RadicalEndpoints
import Api.Lingwave as LingwaveApi
import Components.CRUD.ModelForm as ModelForm
import Components.CRUD.ModelForm.FileUploader as FileUploader
import Components.CRUD.ModelForm.Input as InputComponent
import Components.CRUD.ModelForm.TextField as TextField
import Components.CRUD.ModelForm.TextAreaField as TextAreaField
import Components.CRUD.ModelForm.ToggleField as ToggleField
import Components.Toast as Toast
import Html exposing (..)
import Http
import List.Extra as ListExtra
import Models.Asset as Asset
import Models.FileUpload as FileUpload
import Models.LanguageContent.Radical as Radical
import Module.Admin.Components.BackgroundSelector as BackgroundSelector
import Module.Admin.Components.ContentReviewFields as ContentReviewFields
import Module.Admin.Components.ConceptSelector as ConceptSelector
import Utilities.Expands as Expands


-- All type declarations start here

type Msg
    = ForegroundMsg FileUploader.Msg
    | BackgroundSelectorMsg BackgroundSelector.Msg
    | ConceptSelectorMsg ConceptSelector.Msg
    | SetActive Bool
    | SetUTFCharacter String
    | SetMnemonic String
    | ContentReviewFieldsMsg ContentReviewFields.Msg
    | RemoveToast Toast.Model
    | BackgroundUploadedResponse (Result Error Asset.Model)
    | ForegroundUploadedResponse (Result Error Asset.Model)
    | RadicalCreatedResponse (Result Error Radical.Model)
    | RadicalUpdatedResponse (Result Error Radical.Model)
    | RadicalLoadedResponse (Result Error Radical.Model)
    | Save


type alias Model =
    { loading : Bool
    , toasts : List Toast.Model
    , apiUrl : String
    , expandFields : List Expands.Expand
    , radical : Radical.Model
    , foregroundField : FileUploader.Model
    , utfCharacterField : InputComponent.Model
    , backgroundField : BackgroundSelector.Model
    , mnemonicField : InputComponent.Model
    , activeField : InputComponent.Model
    , conceptSelector: ConceptSelector.Model
    }


-- All state stuff here

initialState : List Expands.Expand -> String -> Token -> Maybe Int -> (Model, Cmd Msg)
initialState expandFields apiUrl token maybeId =
    let
        (backgroundField, backgroundCmd) =
            BackgroundSelector.initialState token apiUrl "radical_background" Nothing

        (conceptSelector, conceptCmd) =
            ConceptSelector.initialState token apiUrl False []

        model =
            { loading = maybeId /= Nothing
            , toasts = []
            , radical = Radical.newModel
            , foregroundField =  FileUploader.initialState "radical_foreground" Nothing
            , utfCharacterField = InputComponent.initialState False "Enter either the Character for this radical, or upload a Foreground SVG" "utf_character"
            , backgroundField = backgroundField
            , mnemonicField = InputComponent.initialState False "Enter the mnemonic for this radical." "mnemonic"
            , activeField = InputComponent.initialState True "Activate the radical and accompanying exercises." "active"
            , conceptSelector = conceptSelector
            }
    in
    ( model
    , Cmd.batch
        [ case maybeId of
            Just id ->
                getRadical token model id
            Nothing ->
                Cmd.none
        , Cmd.map BackgroundSelectorMsg backgroundCmd
        , Cmd.map ConceptSelectorMsg conceptCmd
        ]
    )


setRadical : Model -> Radical.Model -> Bool -> Model
setRadical model radical loading =
    let
        foregroundAsset =
            case (radical.foreground_id, radical.foreground_url) of
                (Just id, Just url) ->
                    Just (Asset.model id url)
                _ ->
                    Nothing

        backgroundAsset =
            case (radical.background_id, radical.background_url) of
                (Just id, Just url) ->
                    Just (Asset.model id url)
                _ ->
                    Nothing

    in
        { model
            | loading = loading
            , radical = radical
            , foregroundField = FileUploader.setAsset model.foregroundField foregroundAsset
            , backgroundField = BackgroundSelector.setAsset model.backgroundField backgroundAsset
            , conceptSelector = ConceptSelector.setConcepts model.conceptSelector radical.concepts
        }


update : Token -> Msg -> Model -> (Model, Cmd Msg)
update token msg model =
    case msg of
        ForegroundMsg subMsg ->
            let
                (foregroundField, foregroundUploaderCmd)
                    = FileUploader.update subMsg model.foregroundField
            in
                ( {model | foregroundField = foregroundField }
                , Cmd.map ForegroundMsg foregroundUploaderCmd
                )

        BackgroundSelectorMsg subMsg ->
            let
                (backgroundField, backgroundSelectorCmd)
                    = BackgroundSelector.update token subMsg model.backgroundField

                radical = model.radical

                updatedRadical =
                    case subMsg of
                        BackgroundSelector.BackgroundSelected asset ->
                            { radical | background_id = Just asset.id }
                        _ ->
                            radical
            in
            ( { model
                | backgroundField = backgroundField
                , radical = updatedRadical
            }
            , Cmd.map BackgroundSelectorMsg backgroundSelectorCmd
            )

        SetActive active ->
            let
                radical = model.radical
            in
                ( { model | radical = { radical | active = active } }
                , Cmd.none
                )

        SetMnemonic mnemonic ->
            let
                radical = model.radical
            in
                ( { model | radical = { radical | mnemonic = Just mnemonic } }
                , Cmd.none
                )

        SetUTFCharacter utfCharacter ->
            let
                radical = model.radical
            in
                ( { model | radical = { radical | utf_character = Just utfCharacter } }
                , Cmd.none
                )

        ContentReviewFieldsMsg subMsg ->
            ( { model
                | radical = ContentReviewFields.update subMsg model.radical
            }
            , Cmd.none
            )

        ConceptSelectorMsg subMsg ->
            let
                (conceptSelector, conceptSelectorCmd) =
                    ConceptSelector.update token subMsg model.conceptSelector
                radical = model.radical
            in
                ( { model
                    | conceptSelector = conceptSelector
                    , radical = { radical | concepts = conceptSelector.selectedConcepts}
                }
                , Cmd.map ConceptSelectorMsg conceptSelectorCmd
                )

        RemoveToast toast ->
            ( { model
                | toasts = ListExtra.remove toast model.toasts
            }
            , Cmd.none
            )

        Save ->
            case validateForm model of
                Just toastTuple ->
                    Toast.appendToast toastTuple model
                Nothing ->
                    ( { model | loading = True }
                    , case model.radical.id of
                        Just id ->
                            Cmd.batch
                                [ batchAssetUploadCommands token model id
                                , updateRadical token model.apiUrl id model.radical
                                ]
                        Nothing ->
                            createRadical token model.apiUrl model.radical
                    )

        BackgroundUploadedResponse (Ok asset) ->
            let
                radical
                    = model.radical
                updatedRadical =
                    { radical
                        | background_id = Just asset.id
                        , background_url = Just asset.url
                    }
            in
            ( { model
                | radical = updatedRadical
                , backgroundField = BackgroundSelector.setAsset model.backgroundField (Just asset)
            }
            , Cmd.none
            )

        BackgroundUploadedResponse (Err error) ->
            Toast.appendToast
                (Toast.createToast Toast.Error RemoveToast <| Api.createErrorText "Error Uploading Background" error)
                { model | loading = False }

        ForegroundUploadedResponse (Ok asset) ->
            let
                radical
                    = model.radical
                updatedRadical =
                    { radical
                        | foreground_id = Just asset.id
                        , foreground_url = Just asset.url
                    }
            in
            ( { model
                | radical = updatedRadical
                , foregroundField = FileUploader.setAsset model.foregroundField (Just asset)
            }
            , Cmd.none
            )

        ForegroundUploadedResponse (Err error) ->
            Toast.appendToast
                (Toast.createToast Toast.Error RemoveToast <| Api.createErrorText "Error Uploading Foreground" error)
                { model | loading = False }

        RadicalCreatedResponse (Ok radical) ->
            case radical.id of
                Just id ->
                    let
                        (updatedModel, toastCmd) =
                            Toast.appendToast
                                (Toast.createToast Toast.Success RemoveToast "Radical Created Successfully")
                                model
                    in
                    ( updatedModel
                    , Cmd.batch
                        [ batchAssetUploadCommands token model id
                        , toastCmd
                        , if model.radical.active then
                            updateRadical token model.apiUrl id model.radical
                        else
                            Cmd.none
                        ]
                    )

                Nothing ->
                    Toast.appendToast
                        (Toast.createToast Toast.Error RemoveToast "Unknown error creating radical")
                        { model | loading = False }

        RadicalCreatedResponse (Err error) ->
            Toast.appendToast
                (Toast.createToast Toast.Error RemoveToast <| Api.createErrorText "Error Creating Radical" error)
                { model | loading = False }

        RadicalUpdatedResponse (Ok _) ->
            Toast.appendToast
                (Toast.createToast Toast.Success RemoveToast "Radical Updated Successfully")
                { model | loading = False }

        RadicalUpdatedResponse (Err error) ->
            Toast.appendToast
                (Toast.createToast Toast.Error RemoveToast <| Api.createErrorText "Error Updating Radical" error)
                { model | loading = False }

        RadicalLoadedResponse (Ok radical) ->
            ( setRadical model radical False
            , Cmd.none
            )

        RadicalLoadedResponse (Err error) ->
            Toast.appendToast
                (Toast.createToast Toast.Error RemoveToast <| Api.createErrorText "Error Loading Radical" error)
                { model | loading = False }


-- Validation right here

validateForm : Model -> Maybe (Toast.Model, Cmd Msg)
validateForm model =
    let
        utfCharacter =
            case model.radical.utf_character of
                Just result ->
                    result
                Nothing ->
                    ""
    in
        if String.length utfCharacter > 1 then
            Just (Toast.createToast Toast.Error RemoveToast "The character cannot be longer than 1 character!")
        else if model.radical.active then

            let
                mnemonic =
                    case model.radical.mnemonic of
                        Just result ->
                            result
                        Nothing ->
                            ""

                backgroundData =
                    (model.radical.background_id, model.backgroundField.selectedBackground.base64)

                foregroundData =
                    ( model.radical.foreground_id
                    , model.foregroundField.base64
                    , String.length utfCharacter == 1
                    )
            in
                if String.length mnemonic == 0 then
                    Just (Toast.createToast Toast.Error RemoveToast "The mnemonic must be filled out before you activate the radical!")
                else
                    case (backgroundData, foregroundData) of
                        ((Nothing, Nothing), _) ->
                            Just (Toast.createToast Toast.Error RemoveToast "The background must be selected before you activate the radical!")
                        (_, (Nothing, Nothing, False)) ->
                            Just (Toast.createToast Toast.Error RemoveToast "You must either upload a foreground image, or enter a character before you activate the radical!")
                        _ ->
                            Nothing

        else
            Nothing


-- All subscriptions below

subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ FileUploader.subscriptions
            |> Sub.map ForegroundMsg
        , BackgroundSelector.subscriptions
            |> Sub.map BackgroundSelectorMsg
        ]


-- View stuff starts here

view : String -> Model -> Html Msg
view title model =
    ModelForm.view title Save model.loading model.toasts
        [ FileUploader.svgView model.foregroundField "Upload a new foreground" model.loading
            |> Html.map ForegroundMsg
        ,  case model.radical.utf_character of
            Just utfCharacter ->
                TextField.view model.utfCharacterField utfCharacter SetUTFCharacter model.loading
            Nothing ->
                TextField.view model.utfCharacterField "" SetUTFCharacter model.loading
        , case model.radical.mnemonic of
            Just mnemonic ->
                TextAreaField.view model.mnemonicField mnemonic SetMnemonic model.loading
            Nothing ->
                TextAreaField.view model.mnemonicField "" SetMnemonic model.loading
        , BackgroundSelector.view model.backgroundField model.loading
            |> Html.map BackgroundSelectorMsg
        , ToggleField.view model.activeField model.radical.active SetActive model.loading
        , ContentReviewFields.view model.radical model.loading
            |> Html.map ContentReviewFieldsMsg
        , ConceptSelector.view model.conceptSelector
            |> Html.map ConceptSelectorMsg
        ]


-- All HTTP stuff here

uploadForeground : Token -> Int -> String -> FileUpload.Model -> Cmd Msg
uploadForeground token radicalId apiUrl file =
    LingwaveApi.uploadFile (RadicalEndpoints.radicalForeground apiUrl radicalId) token file ForegroundUploadedResponse


uploadBackground : Token -> Int -> String -> FileUpload.Model -> Cmd Msg
uploadBackground token radicalId apiUrl file =
    LingwaveApi.uploadFile (RadicalEndpoints.radicalBackground apiUrl radicalId) token file BackgroundUploadedResponse


createRadical : Token -> String -> Radical.Model -> Cmd Msg
createRadical token apiUrl radicalModel =
    LingwaveApi.createModel (RadicalEndpoints.new apiUrl) token (Http.jsonBody (Radical.toCreateJson radicalModel)) Radical.modelDecoder RadicalCreatedResponse


updateRadical : Token -> String -> Int -> Radical.Model -> Cmd Msg
updateRadical token apiUrl id radicalModel =
    LingwaveApi.updateModel (RadicalEndpoints.existing apiUrl [] id) token (Http.jsonBody (Radical.toUpdateJson radicalModel)) Radical.modelDecoder RadicalUpdatedResponse


getRadical : Token -> Model -> Int -> Cmd Msg
getRadical token model id =
    Api.get (RadicalEndpoints.existing model.apiUrl (Expands.toQueryParameters model.expandFields) id) (Just token) Radical.modelDecoder RadicalLoadedResponse

