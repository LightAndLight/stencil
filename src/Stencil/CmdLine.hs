{-# language FlexibleContexts #-}
{-# language GADTs #-}
module Stencil.CmdLine where

import Stencil

import Control.Applicative.Free
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Options.Applicative

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

buildParser :: Steps Text Text a -> Parser (Map Text Text)
buildParser (Pure _) = pure Map.empty
buildParser (Ap f b) =
  case f of
    PromptF name pretty choices Nothing ->
      Map.insert name <$>
      fmap
        Text.pack
        (strOption
          (long (Text.unpack name) <>
           metavar (quoted $ Text.unpack pretty) <>
           help (choicesAndDefault choices Nothing))) <*>
      buildParser b
    PromptF name pretty choices (Just def) ->
      Map.insert name <$>
      fmap
        Text.pack
        (strOption
          (long (Text.unpack name) <>
           metavar (quoted $ Text.unpack pretty) <>
           help (choicesAndDefault choices $ Just def)) <|>
          pure (Text.unpack def)) <*>
      buildParser b
    _ -> empty
  where
    quoted a = "\"" <> a <> "\""
    choicesAndDefault choices def =
      unlines $
        foldMap
          (\choices' ->
             [ "Choices: " ] <>
             fmap ((++"  ") . Text.unpack) (NonEmpty.toList choices'))
          choices <>
        foldMap
          (\def' -> [ "", "Default: " <> Text.unpack def' ])
          def

runStepCmdLine :: (MonadIO m, MonadReader (Map Text Text) m) => StepsF Text Text a -> m a
runStepCmdLine (PromptF name pretty _ _) =
  let
    err =
      error $ "stencil error: required argument '" <> Text.unpack pretty <> "'not provided"
  in
    fromMaybe err . Map.lookup name <$> ask
runStepCmdLine (SetF var content) =
  local (Map.insert var content) $ pure ()
runStepCmdLine (FillTemplateF path template) = ask >>= runFillTemplate path template
runStepCmdLine (LoadTemplateF file) = runLoadTemplate file
runStepCmdLine (CreateFileF file content) = runCreateFile file content
runStepCmdLine (MkDirF path) = runMkDir path
runStepCmdLine (DebugF t) = runDebug t
runStepCmdLine (DebugVariableF name) = ask >>= runDebugVariable name

runStepsCmdLine :: MonadIO m => Steps Text Text a -> Map Text Text -> m a
runStepsCmdLine steps = runReaderT (runAp runStepCmdLine steps)

nonInteractive :: Steps Text Text a -> ParserInfo (Map Text Text)
nonInteractive steps =
  info (strArgument mempty *> strArgument mempty *> buildParser steps <**> helper) fullDesc
