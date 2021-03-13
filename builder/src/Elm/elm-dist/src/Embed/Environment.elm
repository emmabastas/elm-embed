module Embed.Environment exposing (string, maybeString)

{-|

@docs string, maybeString

-}

import Embed
import Embed.Internal
import Json.Decode as Decode
import Json.Encode as Encode


{-| Embed the contents of an environment variable. Fails if the environment
isn't set.

    string "my_name" -- "Mark"

    string "empty" -- ""

    string "non_existent" -- Error

-}
string : String -> Embed.Task String
string name =
    Embed.andThen
        (\m ->
            case m of
                Just s ->
                    Embed.succeed s

                Nothing ->
                    Embed.fail ("Expected environment variable `" ++ name ++ "`")
        )
        (maybeString name)


{-| Like [`string`](#string), but returns `Nothing` if environment variable isn't set.
-}
maybeString : String -> Embed.Task (Maybe String)
maybeString name =
    Embed.Internal.Task "Embed.Environment.maybeString"
        [ Encode.string name ]
        (Decode.map Embed.succeed (Decode.nullable Decode.string))
