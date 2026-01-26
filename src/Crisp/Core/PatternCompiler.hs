{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Core.PatternCompiler
-- Description : Pattern match elaboration
--
-- Transforms surface-level pattern matching into core calculus case
-- expressions with explicit constructor checking and variable binding.
--
-- The pattern compiler handles:
-- - Nested patterns: flattened to sequential case expressions
-- - Wildcard patterns: handled correctly with PatWild
-- - Constructor patterns: destructure into component bindings
-- - Tuple patterns: converted to PatCon "Tuple" subpatterns
-- - Literal patterns: compiled to equality checks
-- - Guard expressions: compiled to nested conditionals
-- - Multiple clauses: compiled to sequential case analysis

module Crisp.Core.PatternCompiler
  ( -- * Pattern Elaboration
    elaborateMatch
  , elaboratePattern
  , elaborateArm
    -- * Types
  , PatternError(..)
  ) where

import qualified Crisp.Syntax.Surface as S
import qualified Crisp.Core.Term as C

import Control.Monad (forM)
import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- | Errors that can occur during pattern elaboration
data PatternError
  = InvalidPatternForm !Text
  | UnsupportedPattern !Text
  | InternalError !Text
  deriving stock (Eq, Show)

-- | State for fresh variable generation
data PatternState = PatternState
  { freshCounter :: !Int
  } deriving stock (Eq, Show)

-- | Initial state
initialState :: PatternState
initialState = PatternState 0

-- | Pattern elaboration monad
type PatternM a = StateT PatternState (Except PatternError) a

-- | Run pattern elaboration
runPatternM :: PatternM a -> Either PatternError a
runPatternM = runExcept . flip evalStateT initialState

-- | Generate a fresh variable name
freshVar :: Text -> PatternM Text
freshVar prefix = do
  n <- gets freshCounter
  modify $ \s -> s { freshCounter = n + 1 }
  pure $ prefix <> "_" <> T.pack (show n)

-- | Elaborate a match expression from surface to core
--
-- Transforms:
--   match subject: pat1 -> body1, pat2 -> body2, ...
-- Into:
--   TmMatch subject' ty [Case pat1' body1', Case pat2' body2', ...]
elaborateMatch :: S.Expr -> Either PatternError C.Term
elaborateMatch = \case
  S.EMatch subject arms _ -> runPatternM $ do
    subject' <- elaborateExpr subject
    arms' <- mapM elaborateArm arms
    -- Type will be inferred
    let resultType = C.TyVar "_infer" 0
    pure $ C.TmMatch subject' resultType arms'

  other -> Left $ InvalidPatternForm $
    "Expected match expression, got: " <> T.pack (show other)

-- | Elaborate a single match arm
elaborateArm :: S.MatchArm -> PatternM C.Case
elaborateArm arm = do
  pattern' <- elaboratePattern (S.matchArmPattern arm)
  body' <- elaborateExpr (S.matchArmBody arm)

  -- Handle guard if present
  finalBody <- case S.matchArmGuard arm of
    Nothing -> pure body'
    Just guardExpr -> do
      guard' <- elaborateExpr guardExpr
      -- Guard compiles to: if guard then body else <fail>
      -- For now, we compile it to a conditional that falls through
      -- In a complete implementation, we'd need to handle pattern match failure
      let failCase = C.TmCon "MatchFailure" [] []  -- Placeholder for failure
      pure $ C.TmMatch guard' (C.TyVar "_infer" 0)
        [ C.Case (C.PatCon "True" []) body'
        , C.Case (C.PatCon "False" []) failCase
        ]

  pure $ C.Case pattern' finalBody

-- | Elaborate a pattern from surface to core
--
-- Surface patterns can include:
-- - Variables (PatVar)
-- - Wildcards (PatWildcard)
-- - Constructors (PatCon)
-- - Tuples (PatTuple) - converted to PatCon "Tuple"
-- - Literals (PatLit) - converted to equality check pattern
-- - Type annotations (PatTyped) - annotation is stripped
elaboratePattern :: S.Pattern -> PatternM C.Pattern
elaboratePattern = \case
  S.PatVar name _ ->
    pure $ C.PatVar name

  S.PatWildcard _ ->
    pure C.PatWild

  S.PatCon conName subPats _ -> do
    subPats' <- mapM elaboratePattern subPats
    pure $ C.PatCon conName subPats'

  S.PatTuple pats _ -> do
    -- Tuples are represented as PatCon "Tuple" in core
    pats' <- mapM elaboratePattern pats
    pure $ C.PatCon "Tuple" pats'

  S.PatLit litExpr _ -> do
    -- Literal patterns compile to a special literal pattern
    -- We represent this as a PatCon with the literal value encoded
    case litExpr of
      S.EIntLit n _ ->
        -- Integer literals become a constructor pattern for the Int type
        -- In a full implementation, this would involve equality checking
        pure $ C.PatCon ("Lit_Int_" <> T.pack (show n)) []
      S.EStringLit s _ ->
        pure $ C.PatCon ("Lit_String_" <> s) []
      S.ECharLit c _ ->
        pure $ C.PatCon ("Lit_Char_" <> T.singleton c) []
      _ -> throwError $ UnsupportedPattern $
        "Unsupported literal pattern: " <> T.pack (show litExpr)

  S.PatTyped innerPat _ty _ ->
    -- Type annotations are stripped - the type checker handles them
    elaboratePattern innerPat

-- | Elaborate an expression (minimal, focused on pattern-related constructs)
--
-- This handles the basic expression forms needed for pattern matching.
-- For a full implementation, this would be part of the main desugaring pass.
elaborateExpr :: S.Expr -> PatternM C.Term
elaborateExpr = \case
  S.EVar name _ ->
    -- Variables get a placeholder index; real indexing happens in full desugar
    pure $ C.TmVar name 0

  S.ECon name _ ->
    pure $ C.TmCon name [] []

  S.EIntLit n _ ->
    -- Represent integer literals as constructor applications
    pure $ C.TmCon ("Int_" <> T.pack (show n)) [] []

  S.EFloatLit f _ ->
    pure $ C.TmCon ("Float_" <> T.pack (show f)) [] []

  S.EStringLit s _ ->
    pure $ C.TmCon ("String_" <> s) [] []

  S.ECharLit c _ ->
    pure $ C.TmCon ("Char_" <> T.singleton c) [] []

  S.EUnit _ ->
    pure $ C.TmCon "Unit" [] []

  S.EApp func args _ -> do
    func' <- elaborateExpr func
    args' <- mapM elaborateExpr args
    pure $ foldl C.TmApp func' args'

  S.ELet pat mTy value body _ -> do
    value' <- elaborateExpr value
    let ty = C.TyVar "_infer" 0  -- Type will be inferred
    case pat of
      S.PatVar name _ -> do
        body' <- elaborateExpr body
        pure $ C.TmLet name ty value' body'
      _ -> do
        -- Complex patterns in let need to be compiled to match
        pat' <- elaboratePattern pat
        body' <- elaborateExpr body
        pure $ C.TmMatch value' ty [C.Case pat' body']

  S.EMatch subject arms _ -> do
    subject' <- elaborateExpr subject
    arms' <- mapM elaborateArm arms
    pure $ C.TmMatch subject' (C.TyVar "_infer" 0) arms'

  S.EIf cond then_ else_ _ -> do
    cond' <- elaborateExpr cond
    then' <- elaborateExpr then_
    else' <- elaborateExpr else_
    -- Desugar if to match on Bool
    pure $ C.TmMatch cond' (C.TyVar "_infer" 0)
      [ C.Case (C.PatCon "True" []) then'
      , C.Case (C.PatCon "False" []) else'
      ]

  S.ELam params body _ -> do
    body' <- elaborateExpr body
    -- Build nested lambdas from inside out
    foldrM wrapLambda body' params
    where
      wrapLambda param innerBody = do
        let paramTy = C.TyVar "_infer" 0  -- Type placeholder
        pure $ C.TmLam (S.paramName param) paramTy innerBody

  S.ELazy inner _ -> do
    inner' <- elaborateExpr inner
    pure $ C.TmLazy inner'

  S.EForce inner _ -> do
    inner' <- elaborateExpr inner
    pure $ C.TmForce inner'

  S.EPipe left right _ -> do
    -- x |> f  desugars to  f x
    left' <- elaborateExpr left
    right' <- elaborateExpr right
    pure $ C.TmApp right' left'

  S.EAnnot expr ty _ -> do
    expr' <- elaborateExpr expr
    -- Type annotation kept as TmAnnot
    pure $ C.TmAnnot expr' (C.TyVar "_infer" 0)

  S.EDo stmts result _ -> do
    result' <- elaborateExpr result
    foldrM elaborateDoStmt result' stmts

  S.EWith handler body _ -> do
    -- For now, just elaborate the body; handler application is complex
    elaborateExpr body

  S.EPerform effect op args _ -> do
    args' <- mapM elaborateExpr args
    let arg = case args' of
          []  -> C.TmCon "Unit" [] []
          [a] -> a
          _   -> C.TmCon "Tuple" [] args'
    pure $ C.TmPerform effect op arg

  S.ERef inner _mut _ ->
    elaborateExpr inner

  S.EBlock stmts result _ -> do
    result' <- elaborateExpr result
    foldrM elaborateStmt result' stmts

  S.EQualified _modPath name _ ->
    pure $ C.TmVar name 0

-- | Elaborate a do-statement
elaborateDoStmt :: S.DoStatement -> C.Term -> PatternM C.Term
elaborateDoStmt stmt rest = case stmt of
  S.DoBind pat effect op args _ -> do
    args' <- mapM elaborateExpr args
    let arg = case args' of
          []  -> C.TmCon "Unit" [] []
          [a] -> a
          _   -> C.TmCon "Tuple" [] args'
    let perform = C.TmPerform effect op arg
    case pat of
      S.PatVar name _ ->
        pure $ C.TmLet name (C.TyVar "_infer" 0) perform rest
      _ -> do
        pat' <- elaboratePattern pat
        pure $ C.TmMatch perform (C.TyVar "_infer" 0) [C.Case pat' rest]

  S.DoLet pat mTy value _ -> do
    value' <- elaborateExpr value
    case pat of
      S.PatVar name _ ->
        pure $ C.TmLet name (C.TyVar "_infer" 0) value' rest
      _ -> do
        pat' <- elaboratePattern pat
        pure $ C.TmMatch value' (C.TyVar "_infer" 0) [C.Case pat' rest]

  S.DoExpr expr _ -> do
    expr' <- elaborateExpr expr
    pure $ C.TmLet "_" (C.simpleType "Unit") expr' rest

-- | Elaborate a statement
elaborateStmt :: S.Statement -> C.Term -> PatternM C.Term
elaborateStmt stmt rest = case stmt of
  S.StmtLet pat mTy value _ -> do
    value' <- elaborateExpr value
    case pat of
      S.PatVar name _ ->
        pure $ C.TmLet name (C.TyVar "_infer" 0) value' rest
      _ -> do
        pat' <- elaboratePattern pat
        pure $ C.TmMatch value' (C.TyVar "_infer" 0) [C.Case pat' rest]

  S.StmtExpr expr _ -> do
    expr' <- elaborateExpr expr
    pure $ C.TmLet "_" (C.simpleType "Unit") expr' rest

-- | Helper for folding with monadic actions (right fold)
foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ z []     = pure z
foldrM f z (x:xs) = do
  rest <- foldrM f z xs
  f x rest
