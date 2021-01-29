{-# LANGUAGE OverloadedStrings #-}
module Init
  ( run
  )
  where


import Prelude hiding (init)
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified Data.Maybe as Maybe
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
            do  approved <- Reporting.ask question
                if approved
                  then init root
                  else
                    do  putStrLn "Exiting without making any changes."
                        return (Right ())



question :: D.Doc
question =
  D.stack
    [ D.reflow "This is what I will do to initialize elm-generate:"
    , D.indent 4 $ D.stack
      [ D.reflow
        "* Create a folder named `elm-generate-scripts` and place some Elm modules in there.\
        \ This is the place where you will write you generators later on."
      , D.reflow
        "* Add `elm-generate-scripts` to your `source-directories` in `elm.json`."

      , D.reflow
        "* Add elm/json to your direct dependencies."
      ]
    , "Ok? [Y/n]: "
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
          let   newSrcDirs =
                  NE.List
                    (Outline.RelativeSrcDir "elm-generate-scripts")
                    (filter
                      ((/=) (Outline.RelativeSrcDir "elm-generate-scripts"))
                      (NE.toList srcDirs))

                elmJson = Maybe.fromMaybe (V.Version 1 1 3) $
                    Map.lookup Pkg.json (Map.union dd di)

                dd_ = Map.insert Pkg.json elmJson dd
                di_ = Map.delete Pkg.json di

                newOutline =
                  Outline.AppOutline ver newSrcDirs dd_ di_ td ti
          in
          do  Dir.createDirectoryIfMissing True "elm-generate-scripts"
              Elm.ElmGenerateScripts.writeModules "elm-generate-scripts"
              Outline.write root (Outline.App newOutline)
              putStrLn "All done!"
              return (Right ())
