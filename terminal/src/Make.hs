{-# LANGUAGE OverloadedStrings #-}
module Make
  ( Flags(..)
  , ReportType(..)
  , run
  , reportType
  , interpreter
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE
import Control.Applicative ((<|>))
import Control.Exception (try)
import qualified System.IO as IO
import qualified System.Process as Proc
import qualified System.Exit as SExit
import qualified System.Directory as Dir
import qualified System.FilePath as FP

import qualified AST.Optimized as Opt
import qualified BackgroundWriter as BW
import qualified Build
import qualified Elm.Details as Details
import qualified Elm.ModuleName as ModuleName
import qualified File
import qualified Generate
import qualified Generate.Html as Html
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff
import Terminal (Parser(..))




-- FLAGS


data Flags =
  Flags
    { _interpreter :: Maybe String
    , _report :: Maybe ReportType
    }


data ReportType
  = Json



-- RUN


type Task a = Task.Task Exit.Make a


run :: () -> Flags -> IO ()
run () (Flags maybeInterpreter report) =
  do  style <- getStyle report
      maybeRoot <- Stuff.findRoot
      Reporting.attemptWithStyle style Exit.makeToReport $
        case maybeRoot of
          Just root -> runHelp root style maybeInterpreter
          Nothing   -> return $ Left $ Exit.MakeNoOutline


runHelp :: FilePath -> Reporting.Style -> Maybe FilePath -> IO (Either Exit.Make ())
runHelp root style maybeInterpreter =
  BW.withScope $ \scope ->
  Stuff.withRootLock root $ Task.run $
  do  details <- Task.eio Exit.MakeBadDetails (Details.load style scope root)
      elmGenerateScriptContents <- listElmGenerateScriptsFolder
      let inputFiles = filter (\p -> p /= "Generate.elm" && p /= "Generate") elmGenerateScriptContents
      case inputFiles of
        [] ->
          Task.throw Exit.MakeNoGeneratorModules

        f:fs ->
          let
            inputPaths = fmap (FP.combine "elm-generate-scripts") (NE.List f fs)
            inputModules = fmap FP.dropExtension (NE.List f fs)
          in
          do  artifacts <- buildPaths style root details inputPaths
              interpreterPath <- getInterpreter maybeInterpreter
              case getNoGenerators artifacts of
                [] ->
                  do  builder <- toBuilder root details artifacts
                      generate style interpreterPath builder (Build.getRootNames artifacts)

                name:names ->
                  Task.throw (Exit.MakeGeneratorModulesWithoutGenerators name names)


listElmGenerateScriptsFolder :: Task.Task Exit.Make [FilePath]
listElmGenerateScriptsFolder =
  Task.eio
    (\_ -> Exit.MakeNoGenerateScriptsFolder)
    (try $ Dir.listDirectory "elm-generate-scripts" :: IO (Either IOError [FilePath]))



-- GET INFORMATION


getStyle :: Maybe ReportType -> IO Reporting.Style
getStyle report =
  case report of
    Nothing -> Reporting.terminal
    Just Json -> return Reporting.json



-- BUILD PROJECTS


buildPaths :: Reporting.Style -> FilePath -> Details.Details -> NE.List FilePath -> Task Build.Artifacts
buildPaths style root details paths =
  Task.eio Exit.MakeCannotBuild $
    Build.fromPaths style root details paths



-- GET MAINLESS


getNoGenerators :: Build.Artifacts -> [ModuleName.Raw]
getNoGenerators (Build.Artifacts _ _ roots modules) =
  Maybe.mapMaybe (getNoGenerator modules) (NE.toList roots)


getNoGenerator :: [Build.Module] -> Build.Root -> Maybe ModuleName.Raw
getNoGenerator modules root =
  case root of
    Build.Inside name ->
      if any (hasGenerator name) modules
      then Nothing
      else Just name

    Build.Outside name _ (Opt.LocalGraph generators _ _) ->
      case generators of
        _:_  -> Nothing
        [] -> Just name


hasGenerator :: ModuleName.Raw -> Build.Module -> Bool
hasGenerator targetName modul =
  case modul of
    Build.Fresh name _ (Opt.LocalGraph generators _ _) ->
      length generators /= 0 && name == targetName

    Build.Cached name mainIsDefined _ ->
      mainIsDefined && name == targetName



-- GENERATE


generate :: Reporting.Style -> FilePath -> B.Builder -> NE.List ModuleName.Raw -> Task ()
generate style interpreter builder names =
  Task.io $
    do  exitCode <- interpret interpreter builder
        File.writeBuilder "elm.js" builder
        -- Dir.createDirectoryIfMissing True (FP.takeDirectory target)
        -- File.writeBuilder target builder
        -- Reporting.reportGenerate style names target



-- INTERPRET


interpret :: FilePath -> B.Builder -> IO SExit.ExitCode
interpret interpreter javascript =
  let
    createProcess = (Proc.proc interpreter []) { Proc.std_in = Proc.CreatePipe }
  in
  Proc.withCreateProcess createProcess $ \(Just stdin) _ _ handle ->
    do  B.hPutBuilder stdin javascript
        IO.hClose stdin
        Proc.waitForProcess handle


getInterpreter :: Maybe String -> Task.Task Exit.Make FilePath
getInterpreter maybeName =
  case maybeName of
    Just name ->
      getInterpreterHelp name (Dir.findExecutable name)

    Nothing ->
      getInterpreterHelp "node` or `nodejs" $
        do  exe1 <- Dir.findExecutable "node"
            exe2 <- Dir.findExecutable "nodejs"
            return (exe1 <|> exe2)


getInterpreterHelp :: String -> IO (Maybe FilePath) -> Task.Task Exit.Make FilePath
getInterpreterHelp name findExe =
  do  maybePath <- Task.io findExe
      case maybePath of
        Just path ->
          return path

        Nothing ->
          Task.throw (Exit.MakeIntepreterNotFound name)



-- TO BUILDER


toBuilder :: FilePath -> Details.Details -> Build.Artifacts -> Task B.Builder
toBuilder root details artifacts =
  Task.mapError Exit.MakeBadGenerate $
    Generate.dev root details artifacts



-- PARSERS


reportType :: Parser ReportType
reportType =
  Parser
    { _singular = "report type"
    , _plural = "report types"
    , _parser = \string -> if string == "json" then Just Json else Nothing
    , _suggest = \_ -> return ["json"]
    , _examples = \_ -> return ["json"]
    }


interpreter :: Parser String
interpreter =
  Parser
    { _singular = "interpreter"
    , _plural = "interpreters"
    , _parser = Just
    , _suggest = \_ -> return ["~/node"]
    , _examples = \_ -> return ["~/node"]
    }
