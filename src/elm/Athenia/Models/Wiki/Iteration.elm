module Athenia.Models.Wiki.Iteration exposing (..)

import Athenia.Models.User.User as User
import Athenia.Utilities.StringHelper as StringHelper
import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)



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
modelDecoder : Decoder Model
modelDecoder =
    JsonDecode.succeed Model
        |> required "id" int
        |> required "content" string
        |> optional "created_by" (maybe User.modelDecoder) Nothing


listDecoder : Decoder (List Model)
listDecoder =
    JsonDecode.list modelDecoder


getContentActionType: String -> String -> ActionType
getContentActionType previousContent newContent =
    let
        maybeFirstDifferentPosition
            = StringHelper.findFirstNoneMatchingStringPosition previousContent newContent
        maybeLastDifferentPosition
            = StringHelper.findLastNoneMatchingStringPosition previousContent newContent
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
                let
                    -- If the first different character and the last different character are the same,
                    -- then the following character will be detected as different, so we need to add a 1 character
                    -- buffer to certain variables
                    buffer =
                        if String.slice firstDifferentPosition 1 newContent == String.slice newContentLastDifferentPosition 1 newContent then
                            1
                        else
                            0
                in
                -- the position where things are different have the same position going forwards and backwards
                if firstDifferentPosition == previousContentLastDifferentPosition + buffer then
                    Add
                        { start_position = firstDifferentPosition
                        , content = String.slice firstDifferentPosition (newContentLastDifferentPosition + buffer) newContent
                        }
                else
                    Replace
                        { start_position = firstDifferentPosition
                        , length = previousContentLastDifferentPosition - firstDifferentPosition
                        , content = String.slice firstDifferentPosition newContentLastDifferentPosition newContent
                        }

            else
                -- the position where things are different have the same position going forwards and backwards
                if firstDifferentPosition == newContentLastDifferentPosition then
                    Remove
                        { start_position = firstDifferentPosition
                        , length = previousContentLastDifferentPosition - firstDifferentPosition
                        }
                else
                    Replace
                        { start_position = firstDifferentPosition
                        , length = previousContentLastDifferentPosition - firstDifferentPosition
                        , content = String.slice firstDifferentPosition newContentLastDifferentPosition newContent
                        }

        _ ->
            NoAction

