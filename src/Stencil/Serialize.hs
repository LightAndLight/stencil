{-# language GADTs #-}
{-# language OverloadedLists, OverloadedStrings #-}
module Stencil.Serialize where

import Control.Applicative.Free (Ap(..), runAp_)
import Data.Text (Text)
import GHC.Exts (fromList)
import Data.Semigroup ((<>))
import qualified Data.Aeson.Encode.Pretty as Json (encodePretty)
import Data.ByteString.Lazy (toStrict)
import Data.Yaml (Value(..), ToJSON, toJSON)
import qualified Data.Yaml.Pretty as Yaml (encodePretty, defConfig)
import Data.Text.Encoding (decodeUtf8)

import qualified Data.List.NonEmpty as NonEmpty

import Stencil (Steps, StepsF(..), renderTemplate)

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
        ConstantF t -> toJSON t
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
        ScriptF tmp -> error "serialization for scripts not implemented"
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
