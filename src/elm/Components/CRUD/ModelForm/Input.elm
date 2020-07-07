module Components.CRUD.ModelForm.Input exposing (..)


-- Type declarations start here

type alias Model =
    { required : Bool
    , label : String
    , name : String
    }


-- Builds the initial state model for this number field
configure : Bool -> String -> String -> Model
configure required label name =
    { required = required
    , label = label
    , name = name
    }
