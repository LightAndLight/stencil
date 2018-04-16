{-# language DeriveLift #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
module Stencil
  ( -- * Steps
    Steps
  , runSteps
  , constant
  , prompt
  , promptRequired
  , promptDefault
  , promptChoice
  , set
  , script
  , fillTemplate
  , loadTemplate
  , createFile
  , mkDir
  , debug
  , debugVariable
    -- * Quasiquoters
  , template
    -- * Steps Internals
  , StepsF(..)
  , runStep
  , runFillTemplate
  , runLoadTemplate
  , runCreateFile
  , runMkDir
  , runDebug
  , runDebugVariable
  , runScript
    -- * Templating Internals
  , Template(..)
  , renderTemplate
  , normalise
  , consolidate
  , fill
  , parseTemplate
  )
where

import Control.Applicative.Free
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Semigroup
import Data.String
import Data.Text (Text)
import Instances.TH.Lift()
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax as TH
import System.Directory
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Text.Trifecta
import Text.Read (readMaybe)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

-- | Documents with holes of type @var@, that can be filled by type @content@
--
-- The 'IsString' instance creates a document with no holes in it, i.e.
--
-- @'consolidate' ('fromString' str) = 'Just' str@
data Template var content
  = Hole !var (Template var content)
  | Optional !var !content (Template var content)
  | Content !content (Template var content)
  | Empty
  deriving (Eq, Show, Ord, Lift)

-- | Display a template in a human-readable format
renderTemplate :: Template Text Text -> Text
renderTemplate (Hole var rest) = "${" <> var <> "}" <> renderTemplate rest
renderTemplate (Optional var def rest) =
  "${" <> var <> "|" <> def <> "}" <> renderTemplate rest
renderTemplate (Content val rest) = val <> renderTemplate rest
renderTemplate Empty = mempty

instance IsString content => IsString (Template var content) where
  fromString str = Content (fromString str) Empty

instance Semigroup (Template v c) where
  Hole a rest <> rest' = Hole a (rest <> rest')
  Content a rest <> rest' = Content a (rest <> rest')
  Optional a b rest <> rest' = Optional a b (rest <> rest')
  Empty <> rest' = rest'

instance Monoid (Template v c) where
  mempty = Empty
  mappend = (<>)

-- | Append adjacent @content@ in a 'Template'
normalise :: Semigroup content => Template var content -> Template var content
normalise Empty = Empty
normalise (Hole a b) = Hole a $ normalise b
normalise (Optional a b c) = Optional a b $ normalise c
normalise (Content a rest) =
  case rest of
    Content b c -> normalise $ Content (a <> b) c
    _ -> Content a $ normalise rest

-- | Fill some holes in a 'Template' given a 'Map' from @var@ to @content@
fill :: Ord var => Map var content -> Template var content -> Template var content
fill _ Empty = Empty
fill env (Hole var rest)
  | Just content <- Map.lookup var env = Content content $ fill env rest
  | otherwise = Hole var $ fill env rest
fill env (Content c rest) = Content c $ fill env rest
fill env (Optional var c rest)
  | Just content <- Map.lookup var env = Content content $ fill env rest
  | otherwise = Optional var c $ fill env rest

-- | Extract a completed document from a template. If there are unfilled holes,
-- outputs 'Nothing'.
consolidate :: Semigroup content => Template var content -> Maybe content
consolidate t =
  case normalise t of
    Optional _ c rest -> consolidate . normalise $ Content c rest
    Content c Empty -> Just c
    _ -> Nothing

-- | Project templating DSL
data StepsF var content a where
  -- | Some text
  ConstantF
    :: Text
    -> StepsF var content Text

  -- | Prompt for input
  PromptF
    :: var
    -> Text
    -> Maybe content
    -> StepsF var content content

  -- | Prompt for input, selecting choices
  PromptChoiceF
    :: var
    -> Text
    -> NonEmpty (Text, Steps var content content)
    -> Maybe (Text, Steps var content content)
    -> StepsF var content content

  -- | Set a variable to a value
  SetF
    :: var
    -> content
    -> StepsF var content ()

  -- | Run a 'Shell' script
  ScriptF
    :: Template var content
    -> StepsF var content ()

  -- | Instantiate a template and write it to a file
  FillTemplateF
    :: Template var Text
    -> Template var content
    -> StepsF var content content

  -- | Load a template from a file
  LoadTemplateF
    :: Text
    -> StepsF var content (Template var content)

  -- | Create a file with some content
  CreateFileF
    :: Text
    -> Text
    -> StepsF var content ()

  -- | Create a directory
  MkDirF
    :: Text
    -> StepsF var content ()

  -- | Print a message
  DebugF
    :: Text
    -> StepsF var content ()

  -- | Print the value of a variable
  DebugVariableF
    :: var
    -> StepsF var content ()

type Steps var content = Ap (StepsF var content)

-- | Some text
constant
  :: Text -- ^ Pretty name
  -> Steps var content Text
constant a = liftAp $ ConstantF a

-- | Prompt for input
prompt
  :: var -- ^ Variable name
  -> Text -- ^ Pretty name
  -> Maybe content -- ^ Default - (pretty name, default)
  -> Steps var content content
prompt a b c = liftAp $ PromptF a b c

-- | Prompt for required input
promptRequired
  :: var -- ^ Variable name
  -> Text -- ^ Pretty name
  -> Steps var content content
promptRequired a b = prompt a b Nothing

-- | Prompt for input with a default
promptDefault
  :: var -- ^ Variable name
  -> Text -- ^ Pretty name
  -> content -- ^ Default
  -> Steps var content content
promptDefault a b c = prompt a b (Just c)

-- | Prompt for input with choices
promptChoice
  :: var -- ^ Variable name
  -> Text -- ^ Pretty name
  -> NonEmpty (Text, Steps var content content) -- ^ Choices - (pretty name, steps which produces choice content)
  -> Maybe (Text, Steps var content content) -- ^ Default
  -> Steps var content content
promptChoice a b c d =
  liftAp $ PromptChoiceF a b c d

-- | Set a variable to a value
set :: var -> content -> Steps var content ()
set a b = liftAp $ SetF a b

-- | Run a 'Shell' script
script
  :: Template var content -- ^ The (templated) shell script to run
  -> Steps var content ()
script = liftAp . ScriptF

-- | Instantiate a template and write it to a file
fillTemplate
  :: Template var Text -- ^ Output path
  -> Template var content -- ^ Template to instantiate
  -> Steps var content content
fillTemplate a b = liftAp $ FillTemplateF a b

-- | Load a template from a file
loadTemplate
  :: Text -- ^ Path to template
  -> Steps var content (Template var content)
loadTemplate a = liftAp $ LoadTemplateF a

-- | Create a file with some content
createFile
  :: Text -- ^ Path to file
  -> Text -- ^ Content
  -> Steps var content ()
createFile a b = liftAp $ CreateFileF a b

-- | Create a directory
mkDir
  :: Text -- ^ Path
  -> Steps var content ()
mkDir a = liftAp $ MkDirF a

-- | Print a message
debug :: Text -> Steps var content ()
debug a = liftAp $ DebugF a

-- | Print the value of a variable
debugVariable :: var -> Steps var content ()
debugVariable a = liftAp $ DebugVariableF a

-- | Render a list of choices with "cabal-init" style dot points
renderChoices
  :: NonEmpty Text -- ^ Choices
  -> Maybe Text -- ^ Default
  -> Text
renderChoices choices def =
  let
    choices' =
      (\(ix, val) -> let ix' = show ix in ((ix', length ix'), val)) <$>
      zip [0..] (NonEmpty.toList choices)
    maxIxlen = maximum $ snd . fst <$> choices'
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

-- | How to run 'FillTemplateF' interactively
runFillTemplate
  :: ( MonadIO m
     , Show var
     , Ord var
     )
  => Template var Text
  -> Template var Text
  -> Map var Text
  -> m Text
runFillTemplate path temp vals = do
  path' <-
    maybe
      (error $ "stencil error: temp could not be completely filled: " <> show vals)
      pure
      (consolidate $ fill vals path)
  temp' <-
    maybe
      (error $ "stencil error: temp could not be completely filled: " <> show vals)
      pure
      (consolidate $ fill vals temp)
  let path'' = Text.unpack path'
  liftIO $ do
    fe <- doesFileExist path''
    de <- doesDirectoryExist path''
    unless (fe || de) $ TIO.writeFile path'' temp'
  pure temp'

-- | How to run 'LoadTemplateF' interactively
runLoadTemplate :: MonadIO m => Text -> m (Template Text Text)
runLoadTemplate file = do
  res <- parseFromFileEx parseTemplate (Text.unpack file)
  case res of
    Success s -> pure s
    Failure e -> error $ "stencil error: parse error in template:\n" <> show (_errDoc e)

-- | How to run 'CreateFileF' interactively
runCreateFile :: MonadIO m => Text -> Text -> m ()
runCreateFile file content =
  liftIO $ do
    let file' = Text.unpack file
    fe <- doesFileExist file'
    de <- doesDirectoryExist file'
    unless (fe || de) $ TIO.writeFile file' content

-- | How to run 'MkDirF' interactively
runMkDir :: MonadIO m => Text -> m ()
runMkDir path = liftIO (createDirectoryIfMissing True $ Text.unpack path)

-- | How to run 'DebugF' interactively
runDebug :: MonadIO m => Text -> m ()
runDebug t = liftIO (TIO.putStrLn $ "debug: " <> t)

-- | How to run 'DebugVariableF' interactively
runDebugVariable :: MonadIO m => Text -> Map Text Text -> m ()
runDebugVariable name vars =
  case Map.lookup name vars of
    Nothing -> error $ "stencil error: variable '" <> Text.unpack name <> "' not set"
    Just value -> liftIO (TIO.putStrLn $ "debug variable: " <> value)

-- | How to run 'ScriptF' interactively
runScript
  :: MonadIO m
  => Template Text Text
  -> Map Text Text
  -> m ()
runScript scr env = do
  scr' <-
    maybe
      (error $ "stencil error: template could not be completely filled: " <> show env)
      pure
      (consolidate $ fill env scr)
  (res, _, _) <- liftIO $ readProcessWithExitCode "sh" ["-c", Text.unpack scr'] ""
  case res of
    ExitSuccess -> pure ()
    ExitFailure n -> error $ "stencil error: script failed with code " <> show n

-- | Run 'StepF' interactively with some initial state
runStep
  :: (MonadState (Map Text Text) m, MonadIO m)
  => StepsF Text Text a
  -> m a
runStep (PromptF name pretty def) = do
  val <- liftIO $ do
    TIO.putStr $ pretty <> "?"
    maybe
      (putStr "\n")
      (\val -> TIO.putStrLn $ " [default: " <> val <> "]")
      def
    loop
  let
    res =
      case def of
        Just content | Text.null val -> content
        _ -> val
  modify (Map.insert name res) $> res
  where
    loop = do
      val <- TIO.getLine
      if Text.null val && isNothing def
        then do
          putStrLn "Please enter a value"
          loop
        else pure val
runStep (PromptChoiceF name pretty choices def) = do
  liftIO $ do
    TIO.putStr $ pretty <> "?"
    maybe
      (putStr "\n")
      (\val -> TIO.putStrLn $ " [default: " <> fst val <> "]")
      def
    liftIO . TIO.putStr $ renderChoices (fst <$> choices) (fst <$> def)
  loop
  where
    loop = do
      val <- liftIO TIO.getLine
      case snd <$> def of
        Just content | Text.null val -> do
          res <- runAp runStep content
          modify (Map.insert name res) $> res
        _ ->
          case readMaybe (Text.unpack val) of
            Nothing -> do
              liftIO $ putStrLn "Please enter an integer"
              loop
            Just n
              | choices' <- NonEmpty.toList choices
              , n < length choices'
              , content <- fmap snd choices' !! n -> do
                  res <- runAp runStep content
                  modify (Map.insert name res) $> res
              | otherwise -> do
                  liftIO $ putStrLn "Invalid selection"
                  loop
runStep (ScriptF scr) = get >>= runScript scr
runStep (FillTemplateF path temp) = get >>= runFillTemplate path temp
runStep (LoadTemplateF file) = runLoadTemplate file
runStep (CreateFileF file content) = runCreateFile file content
runStep (MkDirF path) = runMkDir path
runStep (DebugF t) = runDebug t
runStep (DebugVariableF name) = get >>= runDebugVariable name
runStep (SetF name val) = modify (Map.insert name val) $> ()
runStep (ConstantF t) = pure t

-- | Run steps interactively
runSteps :: Steps Text Text a -> IO a
runSteps s = evalStateT (runAp runStep s) Map.empty

-- | Parse template syntax
--
-- @
-- template = template_piece*
-- template_piece ::= hole | hole_optional | content
--
-- hole ::= "$" "{" identifier "}"
--
-- hole_optional ::= "$" "{" identifier "|" stringLiteral "}"
--
-- content ::= (content_escape_seq | <ascii / {"$", "\\"}> )+
-- content_escape_seq ::= "\" "$" | "\" "\"
--
-- identifier ::= (<ascii / {"|", "}", "\"}>)+
-- @
parseTemplate :: (TokenParsing m, Monad m) => m (Template Text Text)
parseTemplate = someHole <|> content <|> (eof $> Empty)
  where
    someHole = do
      text "${"
      var <- identifier
      whiteSpace
      hole var <|> holeOptional var

    hole var =
      Hole var <$>
      (char '}' *> parseTemplate)

    holeOptional var =
      uncurry Optional <$>
      ((,) var <$> (char '|' *> whiteSpace *> stringLiteral) <* whiteSpace <* char '}') <*>
      parseTemplate

    content =
      Content <$>
      (fmap fold . some $ escapeSeq <|> try (Text.pack . pure <$> noneOf "$")) <*>
      parseTemplate

    escapeSeq = fmap (Text.pack . pure) $ char '\\' *> oneOf "$\\"

    identifier = Text.pack <$> many (noneOf "|}\\")

-- | 'QuasiQuoter' for template syntax.
--
-- @['template'|this string contains a ${variable}]@
--
-- @['template'|this string contains a ${variable | "with a default value"}]@
template :: QuasiQuoter
template =
  QuasiQuoter
  { quoteExp = \input ->
      case parseString parseTemplate mempty input of
        Failure e -> fail $ "error in template: \n" <> show (_errDoc e)
        Success s -> TH.lift s
  , quotePat = const (fail "template cannot be used as a pattern")
  , quoteType = const (fail "template cannot be used as a type")
  , quoteDec = const (fail "template cannot be used as a declaration")
  }
