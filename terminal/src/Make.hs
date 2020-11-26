{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Make
  ( Flags(..)
  , ReportType(..)
  , run
  , reportType
  , interpreterPath
  )
  where
import Debug.Trace (trace)


import qualified Data.ByteString.Builder as B
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as JSON
import qualified Data.Maybe as Maybe
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.NonEmptyList as NE
import qualified Data.Name as Name
import Control.Applicative ((<|>))
import Control.Exception (try)
import qualified Control.Monad
import qualified System.IO as IO
import qualified System.Process as Proc
import qualified System.Exit as SExit
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))
import qualified System.FilePath as FP
import GHC.Word (Word16)

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
import qualified Reporting.Annotation as A
--import qualified Reporting.Anotation as A
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
      inputFiles <- getGeneratorFiles "elm-generate-scripts"
      case inputFiles of
        [] ->
          Task.throw Exit.MakeNoGeneratorModules

        f:fs ->
          let
            inputPaths = fmap (FP.combine "elm-generate-scripts") (NE.List f fs)
          in
          do  artifacts <- buildPaths style root details inputPaths
              case getNoGenerators artifacts of
                [] ->
                  do  (objects, builder) <- toBuilder root details artifacts
                      interpreterPath <- getInterpreter maybeInterpreter
                      generated <- runGenerators interpreterPath builder
                      case generated of
                        Failure e ->
                          Task.throw $ Exit.MakeGeneratorFail $
                            map (\(TaskFailure mn dn m) -> (mn, dn, m)) e

                        Success r ->
                          emitGenerated objects r

                name:names ->
                  Task.throw (Exit.MakeGeneratorModulesWithoutGenerators name names)



-- GET INFORMATION


getStyle :: Maybe ReportType -> IO Reporting.Style
getStyle report =
  case report of
    Nothing -> Reporting.terminal
    Just Json -> return Reporting.json


getGeneratorFiles :: FilePath -> Task [FilePath]
getGeneratorFiles directory =
  Task.eio (\_ -> Exit.MakeNoGenerateScriptsFolder) $
    do  result <- try $ Dir.listDirectory directory :: IO (Either IOError [FilePath])
        return $ fmap
          (filter (\p -> p /= "Generate.elm" && p /= "Generate"))
          result


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

    Build.Outside name _ (Opt.LocalGraph generators _ _ _) ->
      case generators of
        _:_  -> Nothing
        [] -> Just name


hasGenerator :: ModuleName.Raw -> Build.Module -> Bool
hasGenerator targetName modul =
  case modul of
    Build.Fresh name _ (Opt.LocalGraph generators _ _ _) ->
      length generators /= 0 && name == targetName

    Build.Cached name mainIsDefined _ ->
      mainIsDefined && name == targetName


getInterpreter :: Maybe String -> Task FilePath
getInterpreter maybeName =
  case maybeName of
    Just name ->
      getInterpreterHelp name (Dir.findExecutable name)

    Nothing ->
      getInterpreterHelp "node` or `nodejs" $
        do  exe1 <- Dir.findExecutable "node"
            exe2 <- Dir.findExecutable "nodejs"
            return (exe1 <|> exe2)


getInterpreterHelp :: String -> IO (Maybe FilePath) -> Task FilePath
getInterpreterHelp name findExe =
  do  maybePath <- Task.io findExe
      case maybePath of
        Just path ->
          return path

        Nothing ->
          Task.throw (Exit.MakeIntepreterNotFound name)



-- COMPILE GENERATORS


buildPaths :: Reporting.Style -> FilePath -> Details.Details -> NE.List FilePath -> Task Build.Artifacts
buildPaths style root details paths =
  Task.eio Exit.MakeCannotBuild $
    Build.fromPaths style root details paths



toBuilder :: FilePath -> Details.Details -> Build.Artifacts -> Task (Generate.Objects, B.Builder)
toBuilder root details artifacts =
  Task.mapError Exit.MakeBadGenerate $
    Generate.devGetObjects root details artifacts



-- RUN GENERATORS


runGenerators :: FilePath -> B.Builder -> Task.Task x Generated
runGenerators interpreter builder =
  do  result <- interpret interpreter builder
      Task.io $ File.writeBuilder "debug.js" builder
      case JSON.decodeStrict' (TextEncoding.encodeUtf8 result) of
        Just generated -> return generated
        Nothing        -> error "nodejs emited bad json"

        -- Dir.createDirectoryIfMissing True (FP.takeDirectory target)
        -- File.writeBuilder target builder
        -- Reporting.reportGenerate style names target


data Generated
  = Success (Map.Map String (Map.Map String String))
  | Failure [TaskFailure]


data TaskFailure
  = TaskFailure
      { _f_moduleName :: String
      , _f_declarationName :: String
      , _f_message :: String
      }


instance FromJSON Generated where
  parseJSON = JSON.withObject "Generated" $ \v ->
    do  tipe <- v .: "type"
        case tipe of
          "success" -> fmap Success (v .: "results")
          "errors"  -> fmap Failure (v .: "errors")
          _         -> Control.Monad.fail ("Unexpeted type: `" ++ tipe ++ "`")


instance FromJSON TaskFailure where
  parseJSON = JSON.withObject "TaskFailure" $ \v -> TaskFailure
    <$> v .: "moduleName"
    <*> v .: "declarationName"
    <*> v .: "v"



-- EMIT GENERATED


emitGenerated :: Generate.Objects -> Map.Map String (Map.Map String String) -> Task ()
emitGenerated (Generate.Objects _ locals) generated =
  Task.io $
    do
        fmap (\_ -> () ) $ mapM
          (\(moduleName, graph) ->
            if "Generate" `List.isPrefixOf` (Name.toChars moduleName) then
              return ()
            else
              let inputPath = "elm-generate-scripts" </> Name.toChars moduleName <.> "elm"
                  outputFolder = "src/Generated"
                  outputPath = outputFolder </> Name.toChars moduleName <.> "elm"
                  outputModule = "Generated." ++ Name.toChars moduleName
              in
              do  Dir.createDirectoryIfMissing True outputFolder
                  inputContents <- TextIO.readFile inputPath
                  let outputContents = emitModule graph (generated ! (Name.toChars moduleName)) inputContents
                  TextIO.writeFile outputPath outputContents
          )
          (Map.toList locals)


emitModule :: Opt.LocalGraph -> Map.Map String String -> Text -> Text
emitModule (Opt.LocalGraph generators _ _ moduleNameInSrc) generated text =
  let startRow (Opt.Generator _ (r, _)) = r
      sorted = List.sortOn startRow generators
      replacements =
        map
          (\(Opt.Generator (Opt.Global _ name) inSrc) ->
            let result = generated ! (Name.toChars name) in
            (inSrc, Text.pack result)
          )
          sorted
  in
  Text.unlines $ replace 1 replacements (Text.lines text)


replace :: Word16 -> [((Word16, Word16), Text)] -> [Text] -> [Text]
replace currentRow replacements lines =
  case replacements of
    [] ->
      lines

    ((startRow, endRow), s) : xs ->
      let (before, y) = splitAt (fromIntegral $ startRow - currentRow) lines
          after = drop (fromIntegral $ endRow - startRow + 1) y
      in
      before ++ s:replace (endRow+1) xs after



-- INTERPRET


interpret :: FilePath -> B.Builder -> Task.Task x Text
interpret interpreter javascript =
  let
    createProcess =
      (Proc.proc interpreter [])
        { Proc.std_in = Proc.CreatePipe, Proc.std_out = Proc.CreatePipe }
  in
  Task.io $ Proc.withCreateProcess createProcess $ \(Just stdin) (Just stdout) _ handle ->
    do  B.hPutBuilder stdin javascript
        IO.hClose stdin
        exitCode <- Proc.waitForProcess handle
        case exitCode of
          SExit.ExitSuccess   -> TextIO.hGetContents stdout
          SExit.ExitFailure _ -> error "running nodejs"


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


interpreterPath :: Parser String
interpreterPath =
  Parser
    { _singular = "interpreter"
    , _plural = "interpreters"
    , _parser = Just
    , _suggest = \_ -> return ["~/node"]
    , _examples = \_ -> return ["~/node"]
    }
