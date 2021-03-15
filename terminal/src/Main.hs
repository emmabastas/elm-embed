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
  P.fillSep ["elm-embed", P.text (V.toChars V.elmGenerate)]


outro :: P.Doc
outro =
  P.fillSep $ map P.text $ words $
    "To get a more detailed description of each command, add the --help flag."



-- INIT


init :: Terminal.Command
init =
  let
    summary =
      "Initialize elm-embed for this project"

    details =
      "Initialize elm-embed for this project"

    example =
      reflow
        "Creates an `elm-embed-scripts` folder with modules consumed by your\
        \ embedders, also adds this folder to `source-directories` in `elm.json`."
  in
  Terminal.Command "init" (Common summary) details example noArgs noFlags Init.run



-- MAKE


make :: Terminal.Command
make =
  let
    summary =
      "Run the embedders in `elm-embed-scripts` and embed the resulting values"

    details =
      "Run the embedders in `elm-embed-scripts` and embed the resulting values"

    example =
      stack []

    makeFlags =
      flags Make.Flags
        |-- flag "interpreter" Make.interpreterPath "Path to a alternate JS interpreter, like node or nodejs."
        |-- flag "report" Make.reportType "You can say --report=json to get error messages as JSON. This is only really useful if you are an editor plugin. Humans should avoid it!"
  in
  Terminal.Command "run" (Common summary) details example noArgs makeFlags Make.run




-- HELPERS


stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat $ List.intersperse "" docs


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string
