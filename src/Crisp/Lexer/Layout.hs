{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Lexer.Layout
-- Description : Layout rule processing for significant whitespace
--
-- Handles the transformation of raw tokens into a stream with
-- explicit INDENT, DEDENT, and NEWLINE tokens based on indentation.

module Crisp.Lexer.Layout
  ( processLayout
  , LayoutState(..)
  , initialLayoutState
  ) where

import Crisp.Lexer.Token
import Crisp.Syntax.Span

import Data.Text (Text)

-- | State for layout processing
data LayoutState = LayoutState
  { layoutStack     :: ![Int]   -- ^ Stack of indentation levels
  , layoutPending   :: ![Token] -- ^ Pending tokens to emit
  , layoutAtLineStart :: !Bool  -- ^ Are we at the start of a line?
  } deriving stock (Eq, Show)

-- | Initial layout state
initialLayoutState :: LayoutState
initialLayoutState = LayoutState
  { layoutStack = [0]
  , layoutPending = []
  , layoutAtLineStart = True
  }

-- | Process a stream of tokens to add layout tokens
-- This is a post-processing step after basic lexing
processLayout :: Text -> [Token] -> [Token]
processLayout _file tokens = go initialLayoutState tokens []
  where
    go :: LayoutState -> [Token] -> [Token] -> [Token]
    go state [] acc =
      -- At end, emit any remaining dedents
      let dedents = generateFinalDedents (layoutStack state) (getSpanFromAcc acc)
      in reverse (dedents ++ acc)

    go state (tok:rest) acc =
      case tokenKind tok of
        -- Handle newline followed by indentation
        Newline ->
          let (indentToks, newStack) = handleNewline state (tok:rest)
          in case indentToks of
               [] -> go state { layoutStack = newStack } rest acc
               _  -> go state { layoutStack = newStack } rest (indentToks ++ acc)

        -- Regular tokens just pass through
        _ -> go state rest (tok : acc)

    getSpanFromAcc [] = noSpan
    getSpanFromAcc (t:_) = tokenSpan t

-- | Handle a newline and determine what layout tokens to emit
handleNewline :: LayoutState -> [Token] -> ([Token], [Int])
handleNewline state tokens =
  case layoutStack state of
    [] -> ([], [0])
    (currentIndent:_) ->
      -- In a real implementation, we'd look at the next non-whitespace token
      -- to determine the new indentation level
      ([], layoutStack state)

-- | Generate dedent tokens down to level 0 at end of file
generateFinalDedents :: [Int] -> Span -> [Token]
generateFinalDedents stack span' = go stack []
  where
    go [] acc = acc
    go [0] acc = acc
    go (_:xs) acc = go xs (Token Dedent span' : acc)
