module Athenia.Models.Wiki.Iteration exposing (..)

import Athenia.Models.User.User as User
import Athenia.Utilities.StringHelper as StringHelper
import Json.Decode as JsonDecode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as JsonEncode



type ActionType
    = Add AddAction
    | Remove RemoveAction
    | Replace ReplaceAction
    | NoAction


type alias Model =
    { id : Int
    , content : String
    , created_by : Maybe User.Model
    }


type alias AddAction =
    { start_position: Int
    , content: String
    }


type alias RemoveAction =
    { start_position: Int
    , length: Int
    }


type alias ReplaceAction =
    { start_position: Int
    , length: Int
    , content: String
    }


-- Decodes a article model retrieved through the API
modelDecoder : JsonDecode.Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" JsonDecode.int
        |> required "content" JsonDecode.string
        |> optional "created_by" (JsonDecode.maybe User.modelDecoder) Nothing


listDecoder : JsonDecode.Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder


convertAddActionToEncodedValue: AddAction -> JsonEncode.Value
convertAddActionToEncodedValue action =
    JsonEncode.object
        [ ("action", JsonEncode.string "add")
        , ("start_position", JsonEncode.int action.start_position)
        , ("content", JsonEncode.string action.content)
        ]


convertRemoveActionToEncodedValue: RemoveAction -> JsonEncode.Value
convertRemoveActionToEncodedValue action =
    JsonEncode.object
        [ ("action", JsonEncode.string "remove")
        , ("start_position", JsonEncode.int action.start_position)
        , ("length", JsonEncode.int action.length)
        ]


convertReplaceActionToEncodedValue: ReplaceAction -> JsonEncode.Value
convertReplaceActionToEncodedValue action =
    JsonEncode.object
        [ ("action", JsonEncode.string "replace")
        , ("start_position", JsonEncode.int action.start_position)
        , ("length", JsonEncode.int action.length)
        , ("content", JsonEncode.string action.content)
        ]


encodeAction: ActionType -> String
encodeAction action =
    case action of
        NoAction ->
            ""
        Add addAction ->
            JsonEncode.encode 0
                <| convertAddActionToEncodedValue addAction

        Remove removeAction ->
            JsonEncode.encode 0
                <| convertRemoveActionToEncodedValue removeAction

        Replace replaceAction ->
            JsonEncode.encode 0
                <| convertReplaceActionToEncodedValue replaceAction


getActionStartPosition: ActionType -> Maybe Int
getActionStartPosition action =
    case action of
        NoAction ->
            Nothing

        Add actionModel ->
            Just actionModel.start_position

        Remove actionModel ->
            Just actionModel.start_position

        Replace actionModel ->
            Just actionModel.start_position


applyAction: ActionType -> String -> String
applyAction action inputString =
    case action of
        NoAction ->
            inputString

        Add actionModel ->
            String.slice 0 actionModel.start_position inputString ++
            actionModel.content ++
            String.slice actionModel.start_position (String.length inputString) inputString

        Remove actionModel ->
            String.slice 0 actionModel.start_position inputString ++
            String.slice (actionModel.start_position + actionModel.length) (String.length inputString) inputString

        Replace actionModel ->
            String.slice 0 actionModel.start_position inputString ++
            actionModel.content ++
            String.slice (actionModel.start_position + actionModel.length) (String.length inputString) inputString


getContentActionType: String -> String -> ActionType
getContentActionType previousContent newContent =
    let
        maybeFirstDifferentPosition =
            if String.length previousContent == 0 || String.length newContent == 0 then
                Just 0
            else
                StringHelper.findFirstNoneMatchingStringPosition previousContent newContent
        maybeLastDifferentPosition =
            if String.length previousContent == 0 || String.length newContent == 0 then
                Just 0
            else
                StringHelper.findLastNoneMatchingStringPosition previousContent newContent
    in
    case (maybeFirstDifferentPosition, maybeLastDifferentPosition) of
        (Just firstDifferentPosition, Just lastDifferentPosition) ->
            let
                previousContentLastDifferentPosition
                    = (String.length previousContent) - lastDifferentPosition
                newContentLastDifferentPosition
                    = (String.length newContent) - lastDifferentPosition
            in
            if String.length newContent >= String.length previousContent then
                -- the position where things are different have the same position going forwards and backwards
                if firstDifferentPosition == String.length previousContent || firstDifferentPosition >= previousContentLastDifferentPosition then
                    Add
                        { start_position = firstDifferentPosition
                        , content = String.slice firstDifferentPosition (firstDifferentPosition + (String.length newContent) - (String.length previousContent)) newContent
                        }
                else
                    Replace
                        { start_position = firstDifferentPosition
                        , length = previousContentLastDifferentPosition - firstDifferentPosition
                        , content = String.slice firstDifferentPosition newContentLastDifferentPosition newContent
                        }

            else
                -- the position where things are different have the same position going forwards and backwards
                if firstDifferentPosition >= newContentLastDifferentPosition then
                    Remove
                        { start_position = firstDifferentPosition
                        , length = String.length previousContent - String.length newContent
                        }
                else
                    Replace
                        { start_position = firstDifferentPosition
                        , length = previousContentLastDifferentPosition - firstDifferentPosition
                        , content = String.slice firstDifferentPosition newContentLastDifferentPosition newContent
                        }

        _ ->
            NoAction

