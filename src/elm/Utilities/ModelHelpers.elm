module Utilities.ModelHelpers exposing (..)

import Json.Decode as JsonDecode exposing (..)


stringFloatDecoder : Decoder Float
stringFloatDecoder =
  (JsonDecode.string)
      |> JsonDecode.andThen (\val ->
          case String.toFloat val of
              Just f -> JsonDecode.succeed f
              _ -> JsonDecode.fail "Error transforming string encoded float to float"
          )
