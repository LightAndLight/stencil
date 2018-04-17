{-# language GADTs #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
module Stencil.Serialize where

import Control.Applicative ((<|>))
import Control.Applicative.Free (Ap(..), runAp_)
import Control.Monad ((<=<))
import Data.Text (Text)
import GHC.Exts (fromList, toList)
import Data.Semigroup ((<>))
import qualified Data.Aeson.Encode.Pretty as Json (encodePretty)
import Data.ByteString.Lazy (toStrict)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Yaml
  ((.:), (.:?), Object, Parser, Value(..), ToJSON, toJSON, parseJSON,
   parseJSONList, decodeEither, parseEither)
import qualified Data.Yaml.Pretty as Yaml (encodePretty, defConfig)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Trifecta (Result(..), parseString)

import qualified Data.List.NonEmpty as NonEmpty

import Stencil (Steps, StepsF(..), Template, parseTemplate, renderTemplate)

printStencilYaml :: Steps Text Text a -> Text
printStencilYaml = decodeUtf8 . Yaml.encodePretty Yaml.defConfig . toValue

printStencilJson :: Steps Text Text a -> Text
printStencilJson = decodeUtf8 . toStrict . Json.encodePretty . toValue

toValue :: Steps Text Text a -> Value
toValue = toJSON . runAp_ go
  where
    go :: StepsF Text Text b -> [Value]
    go step =
      pure $
      case step of
        ConstantF t -> Object [("constant", toJSON t)]
        PromptF name prettyName def ->
          Object
          [ ( "prompt"
            , Object $
                [ ("name", String name)
                , ("pretty_name", String prettyName)
                ] <>
                maybe mempty (\val -> [("default", String val)]) def
            )
          ]
        PromptChoiceF name prettyName choices def ->
          Object
          [ ( "prompt_choice"
            , Object $
                [ ("name", toJSON name)
                , ("pretty_name", toJSON prettyName)
                , ( "choices"
                  , Object . fromList . NonEmpty.toList $
                    fmap (\(n, scr) -> (n, toValue scr)) choices
                  )
                ] <>
                maybe
                  mempty
                  (\(n, scr) -> [("default", Object $ fromList [(n, toValue scr)])])
                  def
            )
          ]
        SetF name value ->
          Object
          [ ( "set"
            , Object
              [ ("name", String name)
              , ("value", String value)
              ]
            )
          ]
        ScriptF tmp ->
          Object
          [ ( "script"
            , Object
              [ ("content", String $ renderTemplate tmp)
              ]
            )
          ]
        FillTemplateF path content ->
          Object
          [ ( "fill_template"
            , Object
              [ ("path", String $ renderTemplate path)
              , ("content", String $ renderTemplate content)
              ]
            )
          ]
        LoadTemplateF path ->
          Object
          [ ( "load_template"
            , Object
              [ ("path", String path)
              ]
            )
          ]
        CreateFileF path content ->
          Object
          [ ( "create_file"
            , Object
              [ ("path", String path)
              , ("content", String path)
              ]
            )
          ]
        MkDirF path ->
          Object
          [ ( "make_directory"
            , Object
              [ ("path", String path)
              ]
            )
          ]
        DebugF message ->
          Object
          [ ( "debug"
            , Object
              [ ("message", String message)
              ]
            )
          ]
        DebugVariableF variable ->
          Object
          [ ( "debug_variable"
            , Object
              [ ("variable", String variable)
              ]
            )
          ]

data SomeSteps var content
  = SomeStepsUnit (StepsF var content ())
  | SomeStepsText (StepsF var content Text)
  | SomeStepsTemplate (StepsF var content (Template var content))

parseStencilYaml :: Text -> Either String (Steps Text Text ())
parseStencilYaml = parseEither fromValue <=< decodeEither . encodeUtf8

parseStencilJson :: Text -> Either String (Steps Text Text ())
parseStencilJson = parseEither fromValue <=< decodeEither . encodeUtf8

data CommandObject
  = Constant Text
  | Prompt Object
  | PromptChoice Object
  | Set Object
  | Script Object
  | FillTemplate Object
  | LoadTemplate Object
  | CreateFile Object
  | MakeDirectory Object
  | Debug Object
  | DebugVariable Object

fromValue :: Value -> Parser (Steps Text Text ())
fromValue value = do
  values :: [Object] <- parseJSONList value
  steps <- traverse go values
  pure $ toAp steps
  where
    toAp :: [SomeSteps Text Text] -> Steps Text Text ()
    toAp [] = pure ()
    toAp (SomeStepsUnit step : rest) = Ap step (const <$> toAp rest)
    toAp (SomeStepsText step : rest) = Ap step (const <$> toAp rest)
    toAp (SomeStepsTemplate step : rest) = Ap step (const <$> toAp rest)

    toApText :: [SomeSteps Text Text] -> Parser (Steps Text Text Text)
    toApText [] = fail "steps do not yield textual value - found empty"
    toApText [SomeStepsText step] = pure $ Ap step (Pure id)
    toApText (SomeStepsText step : rest) = Ap step . fmap const <$> toApText rest
    toApText (SomeStepsUnit step : rest) =
      fail "steps do not yield textual value - found unit"
    toApText (SomeStepsTemplate step : rest) =
      fail "steps do not yield textual value - found template"

    fromValueText :: Value -> Parser (Steps Text Text Text)
    fromValueText value = do
      values :: [Object] <- parseJSONList value
      steps <- traverse go values
      toApText steps

    go :: Object -> Parser (SomeSteps Text Text)
    go v = do
      p <-
        Constant <$> v .: "constant" <|>
        Prompt <$> v .: "prompt" <|>
        PromptChoice <$> v .: "prompt_choice" <|>
        Set <$> v .: "set" <|>
        Script <$> v .: "script" <|>
        FillTemplate <$> v .: "fill_template" <|>
        LoadTemplate <$> v .: "load_template" <|>
        CreateFile <$> v .: "create_file" <|>
        MakeDirectory <$> v .: "make_directory" <|>
        Debug <$> v .: "debug" <|>
        DebugVariable <$> v .: "debug_variable" <|>
        fail "invalid command"
      case p of
        Constant o -> pure $ SomeStepsText (ConstantF o)
        Prompt o ->
          fmap SomeStepsText $
          PromptF <$> o .: "name" <*> o .: "pretty_name" <*> o .:? "default"
        PromptChoice o ->
          fmap SomeStepsText $ do
            n <- o .: "name"
            pn <- o .: "pretty_name"
            csObj :: Object <- o .: "choices"
            case toList csObj :: [(Text, Value)] of
              [] -> fail "expected non-empty obect"
              c:cs -> do
                cs' <- traverse (\(name, value) -> (,) name <$> fromValueText value) (c :| cs)
                def <- o .:? "default"
                PromptChoiceF n pn cs' <$>
                  maybe
                    (pure Nothing)
                    (\(a, b) -> Just . (,) a <$> fromValueText b)
                    def
        Set o ->
          fmap SomeStepsUnit $ SetF <$> o .: "name" <*> o .: "value"
        Script o ->
          fmap SomeStepsUnit $ do
            c <- o .: "content"
            case parseString parseTemplate mempty c of
              Failure err -> fail $ show err
              Success a -> pure $ ScriptF a
        FillTemplate o ->
          fmap SomeStepsText $ do
            c1 <- o .: "path"
            c2 <- o .: "content"
            c1' <- case parseString parseTemplate mempty c1 of
              Failure err -> fail $ show err
              Success a -> pure a
            c2' <- case parseString parseTemplate mempty c2 of
              Failure err -> fail $ show err
              Success a -> pure a
            pure $ FillTemplateF c1' c2'
        LoadTemplate o ->
          fmap SomeStepsTemplate $ LoadTemplateF <$> o .: "path"
        CreateFile o ->
          fmap SomeStepsUnit $ CreateFileF <$> o .: "path" <*> o .: "content"
        MakeDirectory o ->
          fmap SomeStepsUnit $ MkDirF <$> o .: "path"
        Debug o ->
          fmap SomeStepsUnit $ DebugF <$> o .: "message"
        DebugVariable o ->
          fmap SomeStepsUnit $ DebugVariableF <$> o .: "variable"
