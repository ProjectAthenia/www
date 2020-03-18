module Components.CRUD.ModelForm.FileUploader exposing (..)

import Components.AudioPlayer as AudioPlayer
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import List.Extra as ListHelpers
import Models.Asset as Asset
import Ports.FileReader as FileReader


-- All type declarations are found here

type alias Model =
    { name: String
    , asset: Maybe Asset.Model
    , base64: Maybe String
    , resource: Maybe String
    }


type Msg
    = FileSelected
    | FileRead FileReader.FilePortData


-- All state manipulation stars here


initialState : String -> Maybe Asset.Model -> Model
initialState name maybeAsset =
    { name = name
    , base64 = Nothing
    , asset = maybeAsset
    , resource =
        case maybeAsset of
            Just asset ->
                Just asset.url
            Nothing ->
                Nothing
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FileSelected ->
            ( model
            , FileReader.fileSelected model.name
            )

        FileRead file ->
            if file.fileUploaderId == model.name then
                case ListHelpers.last (String.split "base64," file.contents) of
                    Just base64 ->
                        ( { model
                            | base64 = Just base64
                            , resource = Just file.contents
                            , asset = Nothing
                        }, Cmd.none )

                    Nothing ->
                        (model, Cmd.none) -- Inform of error reading file contents
            else
                (model, Cmd.none) -- Event fired from a different file uploader


subscriptions : Sub Msg
subscriptions =
    FileReader.fileContentRead FileRead


setAsset : Model -> Maybe Asset.Model -> Model
setAsset model maybeAsset =
    { model
        | asset = maybeAsset
        , base64 = Nothing
        , resource =
            case maybeAsset of
                Just asset ->
                    Just asset.url
                Nothing ->
                    Nothing
    }


-- All view stuff found below

inputView : Model -> String -> String -> Bool -> Html Msg
inputView model labelText accepts isDisabled =
    label
        [ for model.name
        , class "btn btn-primary file_uploader"
        ]
        [ text labelText
        , input
             [ type_ "file"
             , class "file_uploader_input"
             , id model.name
             , accept accepts
             , disabled isDisabled
             , on "change" (Decode.succeed FileSelected)
             , value "" -- clear out the file value at all times, so the user can select the same file,
                        -- and still have the change event be fired.
             ]
             []
        ]



svgView : Model -> String -> Bool -> Html Msg
svgView model labelText isDisabled =
    case model.resource of
        Nothing ->
            div [ ]
                [ div [ class "svg_preview" ] []
                , p [] [ text "No SVG Selected"]
                , inputView model labelText ".svg" isDisabled
                ]
        Just resource ->
            div [ ]
                [ img
                    [ class "svg_preview"
                    , src resource
                    ] []
                , p [] [ text "Currently selected SVG" ]
                , inputView model labelText ".svg" isDisabled
                ]


audioView : Model -> Bool -> Html Msg
audioView model isDisabled =
    let
        labelText =
            case model.asset of
                Just asset ->
                    "Replace Existing audio clip."

                Nothing ->
                    case model.base64 of
                        Just base64 ->
                            "Change Selected audio clip."

                        Nothing ->
                            "Upload an audio clip."
    in
        div []
            [ inputView model labelText "audio/*" isDisabled
            , case model.resource of
                Just url ->
                    AudioPlayer.view url
                Nothing ->
                    text ""
            ]

