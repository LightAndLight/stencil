{-# language FlexibleContexts #-}
{-# language GADTs #-}
module Stencil.CmdLine where

import Stencil

import Control.Applicative.Free
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import Options.Applicative
import Turtle.Shell

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

buildParser :: Steps Text Text a -> Parser (Map Text Text)
buildParser (Pure _) = pure Map.empty
buildParser (Ap f b) =
  case f of
    PromptF name pretty choices def -> 
      maybe id (Map.insert name) <$>
      (optional .
       fmap Text.pack $
       case choices of
         Nothing ->
          strOption
          (long (Text.unpack name) <>
            metavar (quoted $ Text.unpack pretty) <>
            help (choicesAndDefault pretty choices def))
         Just choices' ->
          option
           (choiceReader $ Text.unpack <$> choices')
           (long (Text.unpack name) <>
            metavar (quoted $ Text.unpack pretty) <>
            help (choicesAndDefault pretty choices def))) <*>
      buildParser b
    _ -> pure Map.empty
  where
    quoted a = "\"" <> a <> "\""

    choiceReader choices = do
      let choices' = NonEmpty.toList choices
      val <- str
      if val `elem` choices'
        then pure val
        else readerError $ "'" <> val <> "' not one of " <> intercalate ", " choices'

    choicesAndDefault pretty Nothing _ = Text.unpack pretty
    choicesAndDefault _ (Just choices) def =
      unlines $
        [ "Choices: " ] <>
        fmap ((++"  ") . Text.unpack) (NonEmpty.toList choices) <>
        foldMap
          (\def' -> [ "", "Default: " <> Text.unpack def' ])
          def

runStepCmdLine
  :: ( MonadIO m
     , MonadReader (IORef (Map Text Text)) m
     )
  => StepsF Text Text a
  -> m a
runStepCmdLine step@(PromptF name _ _ _) =
  (Map.lookup name <$> getRIO) >>=
  maybe (runStep step) pure
runStepCmdLine (SetF var content) =
  modifyRIO (Map.insert var content)
runStepCmdLine (FillTemplateF path template) = getRIO >>= runFillTemplate path template
runStepCmdLine (LoadTemplateF file) = runLoadTemplate file
runStepCmdLine (CreateFileF file content) = runCreateFile file content
runStepCmdLine (MkDirF path) = runMkDir path
runStepCmdLine (DebugF t) = runDebug t
runStepCmdLine (DebugVariableF name) = getRIO >>= runDebugVariable name
runStepCmdLine (ScriptF script) = getRIO >>= runScript script

runStepsCmdLine :: MonadIO m => Steps Text Text a -> Map Text Text -> m a
runStepsCmdLine steps env =
  liftIO (newIORef env) >>=
  runReaderT (runAp runStepCmdLine steps)

nonInteractive :: Steps Text Text a -> ParserInfo (Map Text Text)
nonInteractive steps =
  info (buildParser steps <**> helper) fullDesc
