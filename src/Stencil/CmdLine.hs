{-# language FlexibleContexts #-}
{-# language GADTs #-}
module Stencil.CmdLine
  ( -- * Optparse Applicative
    cmdLineApp
  , buildParserInfo
  , buildParser
    -- * Internals
  , AppPlan(..)
  , runStepCmdLine
  , runStepsCmdLine
  )
where

import Stencil

import Control.Applicative.Free
import Control.Monad.IO.Class
import Control.Monad.State
import Data.List
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import Options.Applicative

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

data AppPlan
  = AppPlan
  { interactive :: Bool
  , variables :: Map Text Text
  }

-- | Build a command line application that can run steps interactively or via command line
-- arguments
cmdLineApp :: Steps Text Text a -> IO a
cmdLineApp steps = do
  AppPlan i vs <- execParser (buildParserInfo steps)
  if i then runSteps steps else runStepsCmdLine steps vs

-- | Build an optparse-applicative 'ParserInfo' that can collect all the variable required
-- to run some 'Steps'.
--
-- @
-- main = execParser (buildParserInfo steps) >>= runStepsCmdLine
-- @
buildParserInfo :: Steps Text Text a -> ParserInfo AppPlan
buildParserInfo steps =
  info (buildParser steps <**> helper) fullDesc

-- | Build an optparse-applicative 'Parser' that can collect all the variables required
-- to run some 'Steps'
buildParser :: Steps Text Text a -> Parser AppPlan
buildParser steps =
  AppPlan <$>
  switch (short 'n' <> help "non-interactive mode") <*>
  buildVariables steps
  where
    buildVariables :: Steps Text Text a -> Parser (Map Text Text)
    buildVariables (Pure _) = pure Map.empty
    buildVariables (Ap f b) =
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
          buildVariables b
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
     , MonadState (Map Text Text) m
     )
  => StepsF Text Text a
  -> m a
runStepCmdLine step@(PromptF name _ _ _) =
  (Map.lookup name <$> get) >>=
  maybe (runStep step) pure
runStepCmdLine (SetF var content) =
  modify (Map.insert var content)
runStepCmdLine (FillTemplateF path template) = get >>= runFillTemplate path template
runStepCmdLine (LoadTemplateF file) = runLoadTemplate file
runStepCmdLine (CreateFileF file content) = runCreateFile file content
runStepCmdLine (MkDirF path) = runMkDir path
runStepCmdLine (DebugF t) = runDebug t
runStepCmdLine (DebugVariableF name) = get >>= runDebugVariable name
runStepCmdLine (ScriptF script) = get >>= runScript script

runStepsCmdLine :: MonadIO m => Steps Text Text a -> Map Text Text -> m a
runStepsCmdLine steps = evalStateT (runAp runStepCmdLine steps)

