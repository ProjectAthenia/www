-- Port for interacting with the system files
port module Ports.FileReader exposing (..)


type alias FilePortData =
  { contents : String
  , filename : String
  , fileUploaderId : String
  }


port fileSelected : String -> Cmd msg


port fileContentRead : (FilePortData -> msg) -> Sub msg
