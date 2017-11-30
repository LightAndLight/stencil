{-# language DeriveLift #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Stencil where

import Control.Applicative.Free
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Char
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Semigroup
import Data.Text (Text)
import Instances.TH.Lift
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax as TH
import System.Directory
import System.Exit
import Text.Trifecta
import Text.Parser.Token
import Text.Parser.Token.Highlight

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import Data.Void

-- Template var content ~ [Either var content]
data Template var content
  = Hole !var (Template var content)
  | Optional !var !content (Template var content)
  | Content !content (Template var content)
  | Empty
  deriving (Eq, Show, Ord, Lift)

instance Semigroup (Template v c) where
  Hole a rest <> rest' = Hole a (rest <> rest)
  Content a rest <> rest' = Content a (rest <> rest)
  Optional a b rest <> rest' = Optional a b (rest <> rest')
  Empty <> rest' = rest'

instance Monoid (Template v c) where
  mempty = Empty
  mappend = (<>)

normalise :: Semigroup content => Template var content -> Template var content
normalise Empty = Empty
normalise (Hole a b) = Hole a $ normalise b
normalise (Optional a b c) = Optional a b $ normalise c
normalise (Content a rest) =
  case rest of
    Content b c -> normalise $ Content (a <> b) c
    _ -> Content a $ normalise rest

fill :: Ord var => Map var content -> Template var content -> Template var content
fill env Empty = Empty
fill env (Hole var rest)
  | Just content <- Map.lookup var env = Content content $ fill env rest
  | otherwise = Hole var $ fill env rest
fill env (Content c rest) = Content c $ fill env rest
fill env (Optional var c rest)
  | Just content <- Map.lookup var env = Content c $ fill env rest
  | otherwise = Optional var c $ fill env rest

consolidate :: Semigroup content => Template var content -> Maybe content
consolidate t =
  case normalise t of
    Optional _ c rest -> consolidate . normalise $ Content c rest
    Content c Empty -> Just c
    _ -> Nothing

data StepsF var content a where
  PromptF -- ^ Prompt for input
    :: var -- ^ Variable name
    -> Text -- ^ Pretty name
    -> Maybe (NonEmpty content) -- ^ Choices
    -> Maybe content -- ^ Default
    -> StepsF var content content
  SetF -- ^ Set a variable to a value
    :: var -- ^ Variable name
    -> content
    -> StepsF var content ()
  FillTemplateF -- ^ Instantiate a template and write it to a file
    :: Template var Text -- ^ Output path
    -> Template var content -- ^ Template to instantiate
    -> StepsF var content content
  LoadTemplateF -- ^ Load a template from a file
    :: Text -- ^ Path to template
    -> StepsF var content (Template var content)
  CreateFileF -- ^ Create a file with some content
    :: Text -- ^ Path to file
    -> Text -- ^ Content
    -> StepsF var content ()
  MkDirF -- ^ Create a directory
    :: Text -- ^ Path
    -> StepsF var content ()
  DebugF -- ^ Print a message
    :: Text -- ^ Message to print
    -> StepsF var content ()
  DebugVariableF -- ^ Print the value of a variable
    :: var -- ^ Variable to inspect
    -> StepsF var content ()

type Steps var content = Ap (StepsF var content)

prompt
  :: var
  -> Text
  -> Maybe (NonEmpty content)
  -> Maybe content
  -> Steps var content content
prompt a b c d = liftAp $ PromptF a b c d

set :: var -> content -> Steps var content ()
set a b = liftAp $ SetF a b

fillTemplate :: Template var Text -> Template var content -> Steps var content content
fillTemplate a b = liftAp $ FillTemplateF a b

loadTemplate :: Text -> Steps var content (Template var content)
loadTemplate a = liftAp $ LoadTemplateF a

createFile :: Text -> Text -> Steps var content ()
createFile a b = liftAp $ CreateFileF a b

mkDir :: Text -> Steps var content ()
mkDir a = liftAp $ MkDirF a

debug :: Text -> Steps var content ()
debug a = liftAp $ DebugF a

debugVariable :: var -> Steps var content ()
debugVariable a = liftAp $ DebugVariableF a

data Stencil a
  = Stencil
  { _stencil_name :: Text
  , _stencil_steps :: Steps Text Text a
  }

modifyRIO :: (MonadReader (IORef r) m, MonadIO m) => (r -> r) -> m ()
modifyRIO f = do
  env <- ask
  liftIO $ modifyIORef env f

getRIO :: (MonadReader (IORef r) m, MonadIO m) => m r
getRIO = do
  env <- asks readIORef
  liftIO env

renderChoices :: NonEmpty Text -> Maybe Text -> Text
renderChoices choices def =
  let
    choices' =
      (\(ix, val) -> let ix' = show ix in ((ix', length ix'), val)) <$>
      zip [0..] (NonEmpty.toList choices)
    maxIxlen = maximum $ (snd . fst) <$> choices'
  in
  foldMap
    (\((ix, ixlen), val) ->
        (case def of
          Nothing -> Text.replicate (2 + maxIxlen - ixlen) " "
          Just val'
            | val == val' -> "* " <> Text.replicate (maxIxlen - ixlen) " "
            | otherwise ->  Text.replicate (2 + maxIxlen - ixlen) " ") <>
        Text.pack ix <> ") " <> val <> "\n")
    choices'

runFillTemplate
  :: ( MonadIO m
     , Show var
     , Ord var
     )
  => Template var Text
  -> Template var Text
  -> Map var Text
  -> m Text
runFillTemplate path template vals = do
  path' <-
    maybe
      (error $ "stencil error: template could not be completely filled: " <> show vals)
      pure
      (consolidate $ fill vals path)
  template' <-
    maybe
      (error $ "stencil error: template could not be completely filled: " <> show vals)
      pure
      (consolidate $ fill vals template)
  liftIO $ TIO.writeFile (Text.unpack path') template'
  pure template'

runLoadTemplate :: MonadIO m => Text -> m (Template Text Text)
runLoadTemplate file = do
  res <- parseFromFileEx parseTemplate (Text.unpack file)
  case res of
    Success s -> pure s
    Failure e -> error $ "stencil error: parse error in template:\n" <> show (_errDoc e)

runCreateFile :: MonadIO m => Text -> Text -> m ()
runCreateFile file content = liftIO (TIO.writeFile (Text.unpack file) content) $> ()

runMkDir :: MonadIO m => Text -> m ()
runMkDir path = liftIO (createDirectoryIfMissing True $ Text.unpack path) $> ()

runDebug :: MonadIO m => Text -> m ()
runDebug t = liftIO (TIO.putStrLn $ "debug: " <> t) $> ()

runDebugVariable :: MonadIO m => Text -> Map Text Text -> m ()
runDebugVariable name vars =
  case Map.lookup name vars of
    Nothing -> error $ "stencil error: variable '" <> Text.unpack name <> "' not set"
    Just value -> liftIO (TIO.putStrLn $ "debug variable: " <> value) $> ()

runStep
  :: (MonadReader (IORef (Map Text Text)) m, MonadIO m)
  => StepsF Text Text a
  -> m a
runStep (PromptF name pretty choices def) = do
  liftIO $ do
    TIO.putStr $ pretty <> "?"
    maybe
      (putStr "\n")
      (\val -> TIO.putStrLn $ " [default: " <> val <> "]")
      def
  case choices of
    Nothing -> pure ()
    Just choices' -> liftIO . TIO.putStr $ renderChoices choices' def
  loop
  where
    loop = do
      val <- liftIO TIO.getLine
      case def of
        Just content | Text.null val -> modifyRIO (Map.insert name content) $> content
        _ ->
          case choices of
            Nothing -> modifyRIO (Map.insert name val) $> val
            Just choices'
              | choices'' <- NonEmpty.toList choices'
              , n <- read (Text.unpack val)
              , n < length choices''
              , content <- choices'' !! n ->
                  modifyRIO (Map.insert name content) $> content
              | otherwise -> do
                  liftIO $ putStrLn "Invalid selection"
                  loop
runStep (FillTemplateF path template) = getRIO >>= runFillTemplate path template
runStep (LoadTemplateF file) = runLoadTemplate file
runStep (CreateFileF file content) = runCreateFile file content
runStep (MkDirF path) = runMkDir path
runStep (DebugF t) = runDebug t
runStep (DebugVariableF name) = getRIO >>= runDebugVariable name
runStep (SetF name val) = modifyRIO (Map.insert name val) $> ()

runSteps :: Steps Text Text a -> IO a
runSteps s = do
  env <- newIORef Map.empty 
  runReaderT (runAp runStep s) env

identifier :: (TokenParsing m, Monad m) => m Text
identifier = ident identStyle
  where
    identStyle =
      IdentifierStyle
      { _styleName = "identifier"
      , _styleStart = alphaNum
      , _styleLetter = alphaNum <|> oneOf "_-"
      , _styleReserved = ["prompt", "optional", "debug", "set", "variable"]
      , _styleHighlight = Identifier
      , _styleReservedHighlight = ReservedIdentifier
      }

parseTemplate :: (TokenParsing m, Monad m) => m (Template Text Text)
parseTemplate = hole <|> holeOptional <|> content <|> (eof $> Empty)
  where
    hole =
      Hole <$>
      try
        (char '$' *>
         between (char '{' <* whiteSpace) (whiteSpace *> char '}') identifier) <*>
      parseTemplate

    holeOptional =
      uncurry Optional <$>
      (char '$' *>
       braces (liftA2 (,) identifier (symbol "|" *> stringLiteral))) <*>
      parseTemplate

    content =
      Content <$>
      (fmap fold . some $ escapeSeq <|> try (Text.pack . pure <$> noneOf "$")) <*>
      parseTemplate

    escapeSeq = text "\\" *> (text "$" <|> text "\\")

template :: QuasiQuoter
template =
  QuasiQuoter
  { quoteExp = \input -> do
      case parseString parseTemplate mempty input of
        Failure e -> fail $ "error in template: \n" <> show (_errDoc e)
        Success s -> TH.lift s
  , quotePat = const (fail "template cannot be used as a pattern")
  , quoteType = const (fail "template cannot be used as a type")
  , quoteDec = const (fail "template cannot be used as a declaration")
  }
