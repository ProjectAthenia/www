module Components.CRUD.ModelForm.Input exposing (..)


-- Type declarations start here

type alias Model =
    { required : Bool
    , label : String
    , name : String
    }


-- Builds the initial state model for this number field
initialState : Bool -> String -> String -> Model
initialState required label name =
    { required = required
    , label = label
    , name = name
    }
