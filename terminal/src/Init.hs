{-# LANGUAGE OverloadedStrings #-}
module Init
  ( run
  )
  where


import Prelude hiding (init)
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified System.Directory as Dir

import qualified Deps.Solver as Solver
import qualified Elm.Constraint as Con
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Elm.ElmGenerateScripts
import qualified Reporting
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Stuff



-- RUN


run :: () -> () -> IO ()
run () () =
  Reporting.attempt Exit.initToReport $
    do  maybeRoot <- Stuff.findRoot
        case maybeRoot of
          Nothing ->
            return (Left Exit.InitNoOutline)

          Just root ->
            init root



question :: D.Doc
question =
  D.stack
    [ D.fillSep
        ["Hello!"
        ,"Elm","projects","always","start","with","an",D.green "elm.json","file."
        ,"I","can","create","them!"
        ]
    , D.reflow
        "Now you may be wondering, what will be in this file? How do I add Elm files to\
        \ my project? How do I see it in the browser? How will my code grow? Do I need\
        \ more directories? What about tests? Etc."
    , D.fillSep
        ["Check","out",D.cyan (D.fromChars (D.makeLink "init"))
        ,"for","all","the","answers!"
        ]
    , "Knowing all that, would you like me to create an elm.json file now? [Y/n]: "
    ]



-- INIT


init :: FilePath -> IO (Either Exit.Init ())
init root =
  do  outline <- Outline.read root
      case outline of
        Left outlineProblem ->
          return (Left (Exit.InitOutlineProblem outlineProblem))

        Right (Outline.Pkg _) ->
          return (Left (Exit.InitPackage))

        Right (Outline.App (Outline.AppOutline ver srcDirs dd di td ti)) ->
          let newSrcDirs =
                NE.List
                  (Outline.RelativeSrcDir "elm-generate-scripts")
                  (filter
                    ((/=) (Outline.RelativeSrcDir "elm-generate-scripts"))
                    (NE.toList srcDirs))

              newOutline =
                Outline.AppOutline ver newSrcDirs dd di td ti
          in
          do  Dir.createDirectoryIfMissing True "elm-generate-scripts"
              Elm.ElmGenerateScripts.writeModules "elm-generate-scripts"
              Outline.write root (Outline.App newOutline)
              putStrLn "Initialized!"
              return (Right ())

defaults :: Map.Map Pkg.Name Con.Constraint
defaults =
  Map.fromList
    [ (Pkg.core, Con.anything)
    , (Pkg.browser, Con.anything)
    , (Pkg.html, Con.anything)
    ]
