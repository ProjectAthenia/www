-- Utility model for dealing with file uploads to the API
module Models.FileUpload exposing(..)

import Json.Encode as Encode


model : String -> Model
model contents =
    { file_contents = contents
    }


type alias Model =
    { file_contents: String
    }


toJson : Model -> Encode.Value
toJson upload =
    Encode.object
        [ ("file_contents", Encode.string upload.file_contents)
        ]
