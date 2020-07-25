-- Utility model for errors returned from the API
module Models.Error exposing (..)

import Html exposing (..)
import Html.Attributes as Attributes
import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (required, optional)


type alias Model =
    { message : String
    , errors : (List (String, List String))
    }


-- Decodes an error retrieved through the API
decoder : Decoder Model
decoder =
    JsonDecode.succeed Model
        |> required "message" string
        |> optional "errors" (keyValuePairs (list string)) []


unknownErrorResponse : Model
unknownErrorResponse =
    { message = "Unknown Server Error"
    , errors = []
    }


view : Model -> Html msg
view model =
    div [ Attributes.class "errors" ]
        <| List.concat
            [ [ h4 [] [ text model.message ] ]
            , List.map viewErrorLine model.errors
            ]


readError : Model -> String
readError model =
    model.message ++ "\n " ++ readErrorLines model.errors


readErrorLines : List (String, List String) -> String
readErrorLines errors =
    String.join " "
        <| List.map readErrorLine errors


readErrorLine : (String, List String) -> String
readErrorLine (errorKey, errors) =
    errorKey ++ " - " ++ (String.join " - " errors)


viewErrorLine : (String, List String) -> Html msg
viewErrorLine (errorKey, errors) =
    p [] [ text (errorKey ++ " - " ++ (String.join " - " errors)) ]
