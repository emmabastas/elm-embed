module Terminal
  ( app
  , Command(..)
  , Summary(..)
  , Flags, noFlags, flags, (|--)
  , Flag, flag, onOff
  , Parser(..)
  , Args, noArgs, required, optional, zeroOrMore, oneOrMore, oneOf
  , require0, require1, require2, require3, require4, require5
  , RequiredArgs, args, exactly, (!), (?), (...)
  )
  where


import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.FilePath ((</>))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (hPutStr, hPutStrLn, stdout)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.Read as Read

import qualified Elm.Version as V
import Terminal.Internal
import qualified Terminal.Chomp as Chomp
import qualified Terminal.Error as Error



-- COMMAND


_command :: String -> P.Doc -> Args args -> Flags flags -> (args -> flags -> IO ()) -> IO ()
_command details example args_ flags_ callback =
  do  setLocaleEncoding utf8
      argStrings <- Env.getArgs
      case argStrings of
        ["--version"] ->
          do  hPutStrLn stdout (V.toChars V.elmGenerate <> " (compatible with Elm " <> V.toChars V.compiler <> ")")
              Exit.exitSuccess

        chunks ->
          if elem "--help" chunks then
            Error.exitWithHelp Nothing details example args_ flags_

          else
            case snd $ Chomp.chomp Nothing chunks args_ flags_ of
              Right (argsValue, flagValue) ->
                callback argsValue flagValue

              Left err ->
                Error.exitWithError err



-- APP


app :: P.Doc -> P.Doc -> [Command] -> IO ()
app intro outro commands =
  do  setLocaleEncoding utf8
      argStrings <- Env.getArgs
      case argStrings of
        [] ->
          Error.exitWithOverview intro outro commands

        ["--help"] ->
          Error.exitWithOverview intro outro commands

        ["--version"] ->
          do  hPutStrLn stdout (V.toChars V.elmGenerate <> " (compatible with Elm " <> V.toChars V.compiler <> ")")
              Exit.exitSuccess

        command : chunks ->
          do  case List.find (\cmd -> toName cmd == command) commands of
                Nothing ->
                  Error.exitWithUnknown command (map toName commands)

                Just (Command _ _ details example args_ flags_ callback) ->
                  if elem "--help" chunks then
                    Error.exitWithHelp (Just command) details example args_ flags_

                  else
                    case snd $ Chomp.chomp Nothing chunks args_ flags_ of
                      Right (argsValue, flagsValue) ->
                        callback argsValue flagsValue

                      Left err ->
                        Error.exitWithError err



-- AUTO-COMPLETE


_maybeAutoComplete :: [String] -> (Int -> [String] -> IO [String]) -> IO ()
_maybeAutoComplete argStrings getSuggestions =
  if length argStrings /= 3 then
    return ()
  else
    do  maybeLine <- Env.lookupEnv "COMP_LINE"
        case maybeLine of
          Nothing ->
            return ()

          Just line ->
            do  (index, chunks) <- getCompIndex line
                suggestions <- getSuggestions index chunks
                hPutStr stdout (unlines suggestions)
                Exit.exitFailure


getCompIndex :: String -> IO (Int, [String])
getCompIndex line =
  do  maybePoint <- Env.lookupEnv "COMP_POINT"
      case Read.readMaybe =<< maybePoint of
        Nothing ->
          do  let chunks = words line
              return (length chunks, chunks)

        Just point ->
          let
            groups = List.groupBy grouper (zip line [0..])
            rawChunks = drop 1 (filter (all (not . isSpace . fst)) groups)
          in
          return
            ( findIndex 1 point rawChunks
            , map (map fst) rawChunks
            )


grouper :: (Char, Int) -> (Char, Int) -> Bool
grouper (c1, _) (c2, _) =
  isSpace c1 == isSpace c2


isSpace :: Char -> Bool
isSpace char =
  char == ' ' || char == '\t' || char == '\n'


findIndex :: Int -> Int -> [[(Char,Int)]] -> Int
findIndex index point chunks =
  case chunks of
    [] ->
      index

    chunk:cs ->
      let
        lo = snd (head chunk)
        hi = snd (last chunk)
      in
      if point < lo then
        0
      else if point <= hi + 1 then
        index
      else
        findIndex (index + 1) point cs


_complexSuggest :: [Command] -> Int -> [String] -> IO [String]
_complexSuggest commands index strings =
  case strings of
    [] ->
      return (map toName commands)

    command : chunks ->
      if index == 1 then
        return (filter (List.isPrefixOf command) (map toName commands))
      else
        case List.find (\cmd -> toName cmd == command) commands of
          Nothing ->
            return []

          Just (Command _ _ _ _ args_ flags_ _) ->
            fst $ Chomp.chomp (Just (index-1)) chunks args_ flags_



-- FLAGS


{-|-}
noFlags :: Flags ()
noFlags =
  FDone ()


{-|-}
flags :: a -> Flags a
flags =
  FDone


{-|-}
(|--) :: Flags (a -> b) -> Flag a -> Flags b
(|--) =
  FMore



-- FLAG


{-|-}
flag :: String -> Parser a -> String -> Flag (Maybe a)
flag =
  Flag


{-|-}
onOff :: String -> String -> Flag Bool
onOff =
  OnOff



-- FANCY ARGS


{-|-}
args :: a -> RequiredArgs a
args =
  Done


{-|-}
exactly :: RequiredArgs a -> Args a
exactly requiredArgs =
  Args [Exactly requiredArgs]


{-|-}
(!) :: RequiredArgs (a -> b) -> Parser a -> RequiredArgs b
(!) =
  Required


{-|-}
(?) :: RequiredArgs (Maybe a -> b) -> Parser a -> Args b
(?) requiredArgs optionalArg =
  Args [Optional requiredArgs optionalArg]


{-|-}
(...) :: RequiredArgs ([a] -> b) -> Parser a -> Args b
(...) requiredArgs repeatedArg =
  Args [Multiple requiredArgs repeatedArg]


{-|-}
oneOf :: [Args a] -> Args a
oneOf listOfArgs =
  Args (concatMap (\(Args a) -> a) listOfArgs)



-- SIMPLE ARGS


{-|-}
noArgs :: Args ()
noArgs =
  exactly (args ())


{-|-}
required :: Parser a -> Args a
required parser =
  require1 id parser


{-|-}
optional :: Parser a -> Args (Maybe a)
optional parser =
  args id ? parser


{-|-}
zeroOrMore :: Parser a -> Args [a]
zeroOrMore parser =
  args id ... parser


{-|-}
oneOrMore :: Parser a -> Args (a, [a])
oneOrMore parser =
  args (,) ! parser ... parser


{-|-}
require0 :: args -> Args args
require0 value =
  exactly (args value)


{-|-}
require1 :: (a -> args) -> Parser a -> Args args
require1 func a =
  exactly (args func ! a)


{-|-}
require2 :: (a -> b -> args) -> Parser a -> Parser b -> Args args
require2 func a b =
  exactly (args func ! a ! b)


{-|-}
require3 :: (a -> b -> c -> args) -> Parser a -> Parser b -> Parser c -> Args args
require3 func a b c =
  exactly (args func ! a ! b ! c)


{-|-}
require4 :: (a -> b -> c -> d -> args) -> Parser a -> Parser b -> Parser c -> Parser d -> Args args
require4 func a b c d =
  exactly (args func ! a ! b ! c ! d)


{-|-}
require5 :: (a -> b -> c -> d -> e -> args) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Args args
require5 func a b c d e =
  exactly (args func ! a ! b ! c ! d ! e)



-- SUGGEST FILES


{-| Helper for creating custom `Parser` values. It will suggest directories and
file names:

    suggestFiles []             -- suggests any file
    suggestFiles ["elm"]        -- suggests only .elm files
    suggestFiles ["js","html"]  -- suggests only .js and .html files

Notice that you can limit the suggestion by the file extension! If you need
something more elaborate, you can implement a function like this yourself that
does whatever you need!
-}
_suggestFiles :: [String] -> String -> IO [String]
_suggestFiles extensions string =
  let
    (dir, start) =
      FP.splitFileName string
  in
  do  content <- Dir.getDirectoryContents dir
      Maybe.catMaybes
        <$> traverse (isPossibleSuggestion extensions start dir) content


isPossibleSuggestion :: [String] -> String -> FilePath -> FilePath -> IO (Maybe FilePath)
isPossibleSuggestion extensions start dir path =
  if List.isPrefixOf start path then
    do  isDir <- Dir.doesDirectoryExist (dir </> path)
        return $
          if isDir then
            Just (path ++ "/")
          else if isOkayExtension path extensions then
            Just path
          else
            Nothing
  else
    return Nothing


isOkayExtension :: FilePath -> [String] -> Bool
isOkayExtension path extensions =
  null extensions || elem (FP.takeExtension path) extensions

