module Embed.Internal exposing (Task(..))

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


type Task a
    = Task String (List Value) (Decoder (Task a))
    | Done a
    | Fail String
