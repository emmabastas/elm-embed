module Embed exposing (Task, map, andThen, succeed, fail)

{-|

@docs Task, map, andThen, succeed, fail

-}

import Embed.Internal
import Json.Decode as Decode


{-| Functions that create tasks are found in other modules. Two common tasks are:

  - [Embed.Environment.string](/packages/emmabastas/elm-embed/latest/Embed-Environment#string)
  - [Embed.File.read](/packages/emmabastas/elm-embed/latest/Embed-File#string)

When tasks have been created they can be chained and further manipulated by the functions found here.

-}
type alias Task a =
    Embed.Internal.Task a


{-| -}
succeed : a -> Task a
succeed =
    Embed.Internal.Done


{-| -}
fail : String -> Task a
fail =
    Embed.Internal.Fail


{-| Map a value before embedding it.

    import Embed
    import Embed.Environment
    import String

    Embed.map String.toUpper (Embed.Environment.string "foo")

-}
map : (a -> b) -> Task a -> Task b
map f =
    andThen (f >> succeed)


{-| Together with [succeed](#succeed) and [fail](#fail) you can use `andThen`
to map a value in a way that can fail.

    import Embed
    import Embed.Environment
    import String

    portNumber : Embed.Task Int
    portNumber =
        Embed.andThen
            (\s ->
                case String.toInt s of
                    Just n ->
                        Embed.succeed n

                    Nothing ->
                        Embed.fail "Expected a number"
            )
            (Embed.Environment.string "port")

-}
andThen : (a -> Task b) -> Task a -> Task b
andThen f io =
    case io of
        Embed.Internal.Done v ->
            f v

        Embed.Internal.Fail message ->
            Embed.Internal.Fail message

        Embed.Internal.Task taskName args decoder ->
            Embed.Internal.Task taskName args (Decode.map (andThen f) decoder)
