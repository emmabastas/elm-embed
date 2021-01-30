module Embed exposing (Task, andThen, fail, map, succeed)

import Embed.Internal
import Json.Decode as Decode


type alias Task a =
    Embed.Internal.Task a


succeed : a -> Task a
succeed =
    Embed.Internal.Done


fail : String -> Task a
fail =
    Embed.Internal.Fail


map : (a -> b) -> Task a -> Task b
map f =
    andThen (f >> succeed)


andThen : (a -> Task b) -> Task a -> Task b
andThen f io =
    case io of
        Embed.Internal.Done v ->
            f v

        Embed.Internal.Fail message ->
            Embed.Internal.Fail message

        Embed.Internal.Task taskName args decoder ->
            Embed.Internal.Task taskName args (Decode.map (andThen f) decoder)
