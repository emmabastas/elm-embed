module Generate.Environment exposing (getVariable)

import Generate
import Json.Decode as Decode
import Json.Encode as Encode


getVariable : String -> Generate.Task (Maybe String)
getVariable name =
    Generate.Task "Environment.getVariable"
        [ Encode.string name ]
        (Decode.map Generate.succeed (Decode.nullable Decode.string))
