module Embed.Environment exposing (maybeString, string)

import Embed
import Embed.Internal
import Json.Decode as Decode
import Json.Encode as Encode


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


maybeString : String -> Embed.Task (Maybe String)
maybeString name =
    Embed.Internal.Task "Embed.Environment.maybeString"
        [ Encode.string name ]
        (Decode.map Embed.succeed (Decode.nullable Decode.string))
