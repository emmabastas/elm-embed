{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
  where


import Prelude hiding (init)
import qualified Data.List as List
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen ((<>))
import Text.Read (readMaybe)

import qualified Elm.Version as V
import Terminal
import Terminal.Helpers

import qualified Init
import qualified Make



-- MAIN


main :: IO ()
main =
  Terminal.app intro outro
    [ init
    , make
    ]


intro :: P.Doc
intro =
  P.vcat
    [ P.fillSep
        ["Hi,","thank","you","for","trying","out"
        ,P.green "elm-generate"
        ,P.green (P.text (V.toChars V.elmGenerate))
        ,"(compatible with Elm " <> (P.text (V.toChars V.compiler)) <> ")."
        ,"I hope you like it!"
        ]
    , ""
    , P.black "-------------------------------------------------------------------------------"
    , P.black "I highly recommend working through <https://guide.elm-lang.org> to get started."
    , P.black "It teaches many important concepts, including how to use `elm` in the terminal."
    , P.black "-------------------------------------------------------------------------------"
    ]


outro :: P.Doc
outro =
  P.fillSep $ map P.text $ words $
    "Be sure to ask on the Elm slack if you run into trouble! Folks are friendly and\
    \ happy to help out. They hang out there because it is fun, so be kind to get the\
    \ best results!"



-- INIT


init :: Terminal.Command
init =
  let
    summary =
      "Start an Elm project. It creates a starter elm.json file and\
      \ provides a link explaining what to do from there."

    details =
      "The `init` command helps start Elm projects:"

    example =
      reflow
        "It will ask permission to create an elm.json file, the one thing common\
        \ to all Elm projects. It also provides a link explaining what to do from there."
  in
  Terminal.Command "init" (Common summary) details example noArgs noFlags Init.run



-- MAKE


make :: Terminal.Command
make =
  let
    summary =
      "The `make` command generates Elm code from code inside `elm-generates-scripts`:"

    details =
      "The `make` command generates Elm code from code inside `elm-generates-scripts`:"

    example =
      stack []

    makeFlags =
      flags Make.Flags
        |-- flag "interpreter" Make.interpreterPath "Path to a alternate JS interpreter, like node or nodejs."
        |-- flag "report" Make.reportType "You can say --report=json to get error messages as JSON. This is only really useful if you are an editor plugin. Humans should avoid it!"
  in
  Terminal.Command "make" (Common summary) details example noArgs makeFlags Make.run




-- HELPERS


stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat $ List.intersperse "" docs


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string
