module Embed exposing (Task(..), andThen, fail, map, succeed)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)


type Task a
    = Task String (List Value) (Decoder (Task a))
    | Done a
    | Fail String


succeed : a -> Task a
succeed =
    Done


fail : String -> Task a
fail =
    Fail


map : (a -> b) -> Task a -> Task b
map f =
    andThen (f >> Done)


andThen : (a -> Task b) -> Task a -> Task b
andThen f io =
    case io of
        Done v ->
            f v

        Fail message ->
            Fail message

        Task taskName args decoder ->
            Task taskName args (Decode.map (andThen f) decoder)
