{-# language GADTs #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
module Stencil.Serialize where

import Control.Applicative ((<|>))
import Control.Applicative.Free (Ap(..), runAp_)
import Control.Monad ((<=<))
import Data.Text (Text)
import GHC.Exts (fromList)
import Data.Semigroup ((<>))
import qualified Data.Aeson.Encode.Pretty as Json (encodePretty)
import Data.ByteString.Lazy (toStrict)
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
    go v =
      fmap SomeStepsText (ConstantF <$> v .: "constant") <|>
      fmap SomeStepsText (do
                    p <- v .: "prompt"
                    PromptF <$> p .: "name" <*> p .: "pretty_name" <*> p .:? "default") <|>
      fmap SomeStepsText (do
                    p <- v .: "prompt_choice"
                    cs <-
                      p .: "choices" >>=
                      traverse
                        (\case
                          Object [(name, value)] ->
                            (,) name <$> fromValueText value
                          _ -> fail "choice was not a single key: value pair")
                    def <- p .:? "default"
                    PromptChoiceF <$>
                      p .: "name" <*>
                      p .: "pretty-name" <*>
                      pure cs <*>
                      maybe
                        (pure Nothing)
                        (\(a, b) -> Just . (,) a <$> fromValueText b)
                        def) <|>
      fmap SomeStepsUnit (do
                    p <- v .: "set"
                    SetF <$> p .: "name" <*> p .: "value") <|>
      fmap SomeStepsUnit (do
                    p <- v .: "script"
                    c <- p .: "content"
                    case parseString parseTemplate mempty c of
                      Failure err -> fail $ show err
                      Success a -> pure $ ScriptF a) <|>
      fmap SomeStepsText (do
                    p <- v .: "fill_template"
                    c1 <- p .: "path"
                    c2 <- p .: "content"
                    c1' <- case parseString parseTemplate mempty c1 of
                      Failure err -> fail $ show err
                      Success a -> pure a
                    c2' <- case parseString parseTemplate mempty c2 of
                      Failure err -> fail $ show err
                      Success a -> pure a
                    pure $ FillTemplateF c1' c2') <|>
      fmap SomeStepsTemplate (do
                    p <- v .: "load_template"
                    LoadTemplateF <$> p .: "path") <|>
      fmap SomeStepsUnit (do
                    p <- v .: "create_file"
                    CreateFileF <$> p .: "path" <*> p .: "content") <|>
      fmap SomeStepsUnit (do
                    p <- v .: "make_directory"
                    MkDirF <$> p .: "path") <|>
      fmap SomeStepsUnit (do
                    p <- v .: "debug"
                    DebugF <$> p .: "message") <|>
      fmap SomeStepsUnit (do
                    p <- v .: "debug_variable"
                    DebugVariableF <$> p .: "variable")
