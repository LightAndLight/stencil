{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language OverloadedLists #-}
{-# language QuasiQuotes #-}

import Stencil
import Stencil.CmdLine

import Control.Exception
import Control.Monad
import Data.Functor
import Data.Map (Map)
import Data.Monoid
import Data.String
import Data.Text (Text)
import Data.Traversable
import Language.Haskell.Interpreter
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Turtle.Prelude hiding (stderr)

import qualified Data.Map as Map
import qualified Data.Text as Text

initInfo :: ParserInfo ()
initInfo =
  info
    ((strArgument
     (metavar "STENCIL" <>
      help "The stencil to run") $> ()) <**>
     helper)
    fullDesc

handleParseResult' :: Maybe String -> ParserResult a -> IO a
handleParseResult' Nothing res = handleParseResult res
handleParseResult' (Just progName) res =
  case res of
    Failure a -> do
      let (msg, exit) = renderFailure a progName
      case exit of
        ExitSuccess -> putStrLn msg
        _ -> hPutStrLn stderr msg
      exitWith exit
    _ -> handleParseResult res

buildStencils :: IO (Map FilePath (Steps Text Text ()))
buildStencils = do
  stencil_path <- getEnv "STENCIL_PATH"
  stencils <- listDirectory stencil_path
  let mapping = Map.fromList $ fmap (\a -> (a, a)) stencils
  result <- runInterpreter $ do
    set [languageExtensions := [QuasiQuotes]]
    loadModules [stencil_path </> "Stencil.hs"]
    setImports ["Stencil"]
    for mapping $ \path -> do
      content <- liftIO (readFile $ stencil_path </> path)
      interpret content (as :: Steps Text Text ())
  case result of
    Left err -> throw err
    Right res -> pure res

main = do
  stencils <- buildStencils
  args <- getArgs
  case args of
    "init" : stencilName : rest
      | Just stencilProgram <- Map.lookup stencilName stencils -> do
          env <-
            handleParseResult' (Just "haskell/nix") $
            execParserPure defaultPrefs (nonInteractive stencilProgram) rest
          void $ runStepsCmdLine stencilProgram env
      | otherwise -> error "stencil error: unknown template name"
    "init" : rest ->
      void . handleParseResult $ execParserPure defaultPrefs initInfo rest
    _ ->
      void . handleParseResult $
      execParserPure defaultPrefs
      (info
        (subparser
          (command "init" initInfo)
          <**>
          helper)
        fullDesc)
      args
