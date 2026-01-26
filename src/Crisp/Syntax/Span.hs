{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Crisp.Syntax.Span
-- Description : Source location tracking
--
-- Provides types for tracking source positions and spans throughout
-- the compilation pipeline, enabling accurate error reporting.

module Crisp.Syntax.Span
  ( -- * Position
    Position(..)
  , mkPosition
  , startPosition
    -- * Span
  , Span(..)
  , mkSpan
  , pointSpan
  , mergeSpans
  , noSpan
    -- * Spanned values
  , Spanned(..)
  , withSpan
  , unspan
  , mapSpanned
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A position in source code (line and column, both 1-indexed)
data Position = Position
  { posLine   :: !Int
  , posColumn :: !Int
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Create a new position
mkPosition :: Int -> Int -> Position
mkPosition = Position

-- | The starting position (1, 1)
startPosition :: Position
startPosition = Position 1 1

-- | A span in source code (start and end positions)
data Span = Span
  { spanStart :: !Position
  , spanEnd   :: !Position
  , spanFile  :: !Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Create a new span
mkSpan :: Position -> Position -> Text -> Span
mkSpan = Span

-- | Create a zero-width span at a position (for synthetic nodes)
pointSpan :: Position -> Text -> Span
pointSpan pos = Span pos pos

-- | Merge two spans into one that covers both
mergeSpans :: Span -> Span -> Span
mergeSpans s1 s2 = Span
  { spanStart = min (spanStart s1) (spanStart s2)
  , spanEnd   = max (spanEnd s1) (spanEnd s2)
  , spanFile  = spanFile s1
  }

-- | A placeholder span for generated code
noSpan :: Span
noSpan = Span startPosition startPosition "<generated>"

-- | A value with an associated source span
data Spanned a = Spanned
  { spannedValue :: !a
  , spannedSpan  :: !Span
  } deriving stock (Eq, Show, Functor, Generic)

-- | Wrap a value with a span
withSpan :: a -> Span -> Spanned a
withSpan = Spanned

-- | Extract the value from a spanned wrapper
unspan :: Spanned a -> a
unspan = spannedValue

-- | Transform the value inside a spanned wrapper
mapSpanned :: (a -> b) -> Spanned a -> Spanned b
mapSpanned = fmap
