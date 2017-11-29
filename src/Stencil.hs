{-# language DeriveFunctor #-}
{-# language DeriveLift #-}
{-# language FlexibleContexts #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Stencil where

import Control.Applicative
import Control.Monad.Free
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
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax as TH
import Instances.TH.Lift
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

data StepsF var content a
  = PromptF -- ^ Prompt for input
      var -- ^ Variable name
      Text -- ^ Pretty name
      (Maybe (NonEmpty content)) -- ^ Choices
      (Maybe content) -- ^ Default
      (content -> a)
  | SetF var content a
  | FillTemplateF -- ^ Instantiate a template
      (Template var content) -- ^ Template to instantiate
      (content -> a)
  | LoadTemplateF -- ^ Load a template from a file
      Text -- ^ Path to template
      (Template var content -> a)
  | CreateFileF -- ^ Create a file with some content
      Text -- ^ Path to file
      Text -- ^ Content
      a
  | MkDirF -- ^ Create a directory
      Text -- ^ Path
      a
  | DebugF -- ^ Print a message
      Text -- ^ Message to print
      a -- ^ Continuation
  | DebugVariableF -- ^ Print the value of a variable
      var -- ^ Variable to inspect
      a -- ^ Continuation
  | EndF -- ^ End
  deriving Functor

type Steps var content = Free (StepsF var content)

prompt
  :: var
  -> Text
  -> Maybe (NonEmpty content)
  -> Maybe content
  -> Steps var content content
prompt a b c d = liftF $ PromptF a b c d id

set :: var -> content -> Steps var content ()
set a b = liftF $ SetF a b ()

fillTemplate :: Template var content -> Steps var content content
fillTemplate a = liftF $ FillTemplateF a id

loadTemplate :: Text -> Steps var content (Template var content)
loadTemplate a = liftF $ LoadTemplateF a id

createFile :: Text -> Text -> Steps var content ()
createFile a b = liftF $ CreateFileF a b ()

mkDir :: Text -> Steps var content ()
mkDir a = liftF $ MkDirF a ()

debug :: Text -> Steps var content ()
debug a = liftF $ DebugF a ()

debugVariable :: var -> Steps var content ()
debugVariable a = liftF $ DebugVariableF a ()

end :: Steps var content a
end = liftF EndF

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

runStep
  :: (MonadReader (IORef (Map Text Text)) m, MonadIO m)
  => StepsF Text Text a
  -> m a
runStep (PromptF name pretty choices def next) = do
  liftIO $ do
    TIO.putStr $ pretty <> "?"
    maybe
      (putStr "\n")
      (\val -> TIO.putStrLn $ " [default: " <> val <> "]")
      def
  case choices of
    Nothing -> pure ()
    Just choices' ->
      let
        choices'' =
          (\(ix, val) -> let ix' = show ix in ((ix', length ix'), val)) <$>
          zip [0..] (NonEmpty.toList choices')
        maxIxlen = maximum $ (snd . fst) <$> choices''
      in
      liftIO $
      traverse_
        (\((ix, ixlen), val) -> do
           putStr $
             maybe
               (replicate (2 + maxIxlen - ixlen) ' ')
               (\val' ->
                  if val == val'
                  then "* " <> replicate (maxIxlen - ixlen) ' '
                  else replicate (2 + maxIxlen - ixlen) ' ')
               def
           putStrLn $ ix <> ") " <> Text.unpack val)
        choices''
  loop
  where
    loop = do
      val <- liftIO TIO.getLine
      case def of
        Just content | Text.null val -> modifyRIO (Map.insert name content) $> next content
        _ ->
          case choices of
            Nothing -> modifyRIO (Map.insert name val) $> next val
            Just choices'
              | choices'' <- NonEmpty.toList choices'
              , n <- read (Text.unpack val)
              , n < length choices''
              , content <- choices'' !! n ->
                  modifyRIO (Map.insert name content) $> next content
              | otherwise -> do
                  liftIO $ putStrLn "Invalid selection"
                  loop
runStep (FillTemplateF template next) = do
  vals <- getRIO
  maybe
    (error $ "stencil error: template could not be completely filled: " <> show vals)
    (pure . next)
    (consolidate $ fill vals template)
runStep (LoadTemplateF file next) = do
  res <- parseFromFileEx parseTemplate (Text.unpack file)
  case res of
    Success s -> pure $ next s
    Failure e -> error $ "stencil error: parse error in template:\n" <> show (_errDoc e)
runStep (CreateFileF file content next) =
  liftIO (TIO.writeFile (Text.unpack file) content) $> next
runStep (MkDirF path next) =
  liftIO (createDirectoryIfMissing True $ Text.unpack path) $> next
runStep (DebugF t next) = liftIO (TIO.putStrLn $ "debug: " <> t) $> next
runStep (DebugVariableF name next) = do
  vars <- getRIO
  case Map.lookup name vars of
    Nothing -> error $ "stencil error: variable '" <> Text.unpack name <> "' not set"
    Just value -> liftIO (TIO.putStrLn $ "debug variable: " <> value) $> next
runStep (SetF name val next) = modifyRIO (Map.insert name val) $> next
runStep EndF = liftIO exitSuccess

runSteps :: Steps Text Text a -> IO a
runSteps s = do
  env <- newIORef Map.empty 
  runReaderT (foldFree runStep s) env

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
