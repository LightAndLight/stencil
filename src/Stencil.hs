module Stencil where

import Data.Monoid
import Data.Semigroup
import Data.Text (Text)
import Data.Map (Map)

-- Template var content ~ [Either var content]
data Template var content
  = Hole !var (Template var content)
  | Optional !var !content (Template var content)
  | Content !content (Template var content)
  | Empty
  deriving (Eq, Show, Ord)

instance Semigroup (Template v c) where
  Hole a rest <> rest' = Hole a (rest <> rest)
  Content a rest <> rest' = Content a (rest <> rest)
  Optional a b rest <> rest' = Content a b (rest <> rest)
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

fill :: Eq var => var -> content -> Template var content -> Template var content
fill var content Empty = Empty
fill var content (Hole var' rest)
  | var == var' = Content content $ fill var content rest
  | otherwise = Hole var $ fill var content rest
fill var content (Content c rest) = Content c $ fill var content rest
fill var content (Optional var' c rest)
  | var == var' = Content c $ fill var content rest
  | otherwise = Optional var' c $ fill var content rest

consolidate :: Semigroup content => Template var content -> Maybe content
consolidate t =
  case normalise t of
    Optional _ c rest -> normalise $ Content c rest
    Content c Empty -> Just c
    _ -> Nothing

data Steps var content
  = Prompt -- ^ Prompt for input
      var -- ^ Variable name
      Text -- ^ Variable description
      [content] -- ^ Choices
      (content -> Steps var content) -- ^ Continuation
  | PromptOptional -- ^ Prompt for optional input
      var -- ^ Variable name
      Text -- ^ Variable description
      [content] -- ^ Choices
      content -- ^ Default value
      (Maybe content -> Steps var content) -- ^ Continuation
  | FillTemplate -- ^ Instantiate a template
      (Map var content) -- ^ Variable values
      (Template var content) -- ^ Template to instantiate
      (content -> Steps var content) -- ^ Continuation
  | Script Void (Steps var content) -- ^ Run a shell script
  | End -- ^ End

data Stencil
  = Stencil
  { _stencil_name :: Text
  , _stencil_steps :: Steps Text Text
  }
