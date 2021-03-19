module Generated.BasicEmbedders exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Embed
import Set exposing (Set)


int : Int
int =
    1337


float : Float
float =
    3.14


char : Char
char =
    '*'


string : String
string =
    "Hello, World!"


unit : ()
unit =
    ()


tuple2 : ( Int, Float )
tuple2 =
    (-1
    , -1
    )


tuple3 : ( Char, String, () )
tuple3 =
    ('A'
    , ""
    , ()
    )


emptyRecord : {}
emptyRecord =
    { 
    }


record : { foo : Int, bar : Char }
record =
    { bar = 'A'
    , foo = 0
    }


emptyList : (List Int)
emptyList =
    [ 
    ]


list : (List Int)
list =
    [ 1
    , 2
    , 3
    ]


array : (Array Int)
array =
    Array.fromList
        [ 1
        , 2
        , 3
        ]


dict : (Dict String Int)
dict =
    Dict.fromList
        [ ("bar"
          , 2
          )
        , ("baz"
          , 3
          )
        , ("foo"
          , 4
          )
        ]


set : (Set Int)
set =
    Set.fromList
        [ 1
        , 2
        , 3
        ]


customType : (Maybe Int)
customType =
    Just
        0
