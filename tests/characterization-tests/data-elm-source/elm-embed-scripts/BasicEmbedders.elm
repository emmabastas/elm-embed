module BasicEmbedders exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Embed
import Set exposing (Set)


int : Embed.Task Int
int =
    Embed.succeed 1337


float : Embed.Task Float
float =
    Embed.succeed 3.14


char : Embed.Task Char
char =
    Embed.succeed '*'


string : Embed.Task String
string =
    Embed.succeed "Hello, World!"


unit : Embed.Task ()
unit =
    Embed.succeed ()


tuple2 : Embed.Task ( Int, Float )
tuple2 =
    Embed.succeed ( -1, -1 )


tuple3 : Embed.Task ( Char, String, () )
tuple3 =
    Embed.succeed ( 'A', "", () )


emptyRecord : Embed.Task {}
emptyRecord =
    Embed.succeed {}


record : Embed.Task { foo : Int, bar : Char }
record =
    Embed.succeed
        { foo = 0
        , bar = 'A'
        }


emptyList : Embed.Task (List Int)
emptyList =
    Embed.succeed []


list : Embed.Task (List Int)
list =
    Embed.succeed [ 1, 2, 3 ]


array : Embed.Task (Array Int)
array =
    Embed.succeed (Array.fromList [ 1, 2, 3 ])


dict : Embed.Task (Dict String Int)
dict =
    [ ( "foo", 1 )
    , ( "bar", 2 )
    , ( "baz", 3 )
    , ( "foo", 4 )
    ]
        |> Dict.fromList
        |> Embed.succeed


set : Embed.Task (Set Int)
set =
    Embed.succeed (Set.fromList [ 1, 2, 3, 1 ])


customType : Embed.Task (Maybe Int)
customType =
    Embed.succeed (Just 0)
