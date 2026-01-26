{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Types.Exhaustive
-- Description : Pattern exhaustiveness checking
--
-- Implements pattern exhaustiveness checking to ensure all pattern matches
-- cover all possible cases, preventing runtime match failures.
--
-- The algorithm uses a pattern matrix approach based on Maranget's work
-- "Warnings for Pattern Matching" to determine:
-- 1. Whether a set of patterns is exhaustive
-- 2. Which patterns are missing (if not exhaustive)
-- 3. Which patterns are redundant
--
-- Key concepts:
-- - A pattern matrix represents the patterns to check
-- - Specialization narrows the matrix for a specific constructor
-- - Default extraction handles wildcard/variable patterns

module Crisp.Types.Exhaustive
  ( -- * Exhaustiveness checking
    isExhaustive
  , missingPatterns
  , hasRedundantPatterns
  , checkExhaustive
    -- * Type information
  , TypeInfo(..)
  , ConstructorInfo(..)
  , ArgInfo(..)
  , TypeEnv(..)
    -- * Errors
  , ExhaustivenessError(..)
  ) where

import Crisp.Core.Term

import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Information about a type for exhaustiveness checking
data TypeInfo = TypeInfo
  { typeInfoName :: !Text
    -- ^ Name of the type
  , typeInfoConstructors :: ![ConstructorInfo]
    -- ^ All constructors of the type
  } deriving stock (Eq, Show)

-- | Information about a constructor
data ConstructorInfo = ConstructorInfo
  { constructorName :: !Text
    -- ^ Name of the constructor
  , constructorArgs :: ![ArgInfo]
    -- ^ Arguments of the constructor
  } deriving stock (Eq, Show)

-- | Information about a constructor argument
data ArgInfo = ArgInfo
  { argName :: !Text
    -- ^ Name of the argument (for named fields)
  , argType :: !Type
    -- ^ Type of the argument
  } deriving stock (Eq, Show)

-- | Type environment mapping type names to their information
data TypeEnv = TypeEnv
  { typeEnvTypes :: ![(Text, TypeInfo)]
    -- ^ All known types
  } deriving stock (Eq, Show)

-- | Errors during exhaustiveness checking
data ExhaustivenessError
  = NonExhaustiveMatch ![Text]
    -- ^ Match is not exhaustive, lists missing patterns
  | RedundantPatterns ![Int]
    -- ^ Some patterns are redundant, lists their indices
  | UnknownConstructor !Text
    -- ^ Referenced an unknown constructor
  | UnknownType !Text
    -- ^ Referenced an unknown type
  deriving stock (Eq, Show)

-- | A pattern row in the matrix
type PatternRow = [Pattern]

-- | A pattern matrix for exhaustiveness checking
type PatternMatrix = [PatternRow]

-- | Check if a set of patterns is exhaustive for a given type
isExhaustive :: TypeEnv -> TypeInfo -> [Pattern] -> Bool
isExhaustive env typeInfo patterns =
  let matrix = map (: []) patterns  -- Each pattern as a single-column row
  in isUseful env [typeInfo] (wildcardRow 1) matrix == False
  where
    wildcardRow n = replicate n PatWild

-- | Find missing patterns for a non-exhaustive match
missingPatterns :: TypeEnv -> TypeInfo -> [Pattern] -> [Text]
missingPatterns _env typeInfo patterns =
  let constructors = typeInfoConstructors typeInfo
      covered = coveredConstructors patterns
      allCons = Set.fromList $ map constructorName constructors
      missing = Set.difference allCons covered
  in map formatMissing $ Set.toList missing
  where
    formatMissing conName =
      case find (\c -> constructorName c == conName) (typeInfoConstructors typeInfo) of
        Just con | not (null (constructorArgs con)) ->
          conName <> "(" <> T.intercalate ", " (replicate (length (constructorArgs con)) "_") <> ")"
        _ -> conName

-- | Get the set of constructors covered by the patterns
coveredConstructors :: [Pattern] -> Set Text
coveredConstructors = Set.fromList . concatMap getConstructors
  where
    getConstructors = \case
      PatCon name _ -> [name]
      PatVar _ -> []  -- Variable doesn't cover specific constructors
      PatWild -> []   -- Wildcard doesn't cover specific constructors

-- | Check if any patterns are redundant
hasRedundantPatterns :: TypeEnv -> TypeInfo -> [Pattern] -> Bool
hasRedundantPatterns env typeInfo patterns =
  any id $ zipWith checkRedundant [0..] patterns
  where
    checkRedundant idx pat =
      let preceding = take idx patterns
          matrix = map (: []) preceding
      in not (isUseful env [typeInfo] [pat] matrix)

-- | Full exhaustiveness check returning detailed results
checkExhaustive :: TypeEnv -> TypeInfo -> [Pattern] -> Either ExhaustivenessError ()
checkExhaustive env typeInfo patterns = do
  -- Check for non-exhaustiveness
  let missing = missingPatterns env typeInfo patterns
  if not (null missing)
    then Left $ NonExhaustiveMatch missing
    else do
      -- Check for redundant patterns
      let redundant = findRedundantIndices env typeInfo patterns
      if not (null redundant)
        then Left $ RedundantPatterns redundant
        else Right ()

-- | Find indices of redundant patterns
findRedundantIndices :: TypeEnv -> TypeInfo -> [Pattern] -> [Int]
findRedundantIndices env typeInfo patterns =
  [ idx | (idx, pat) <- zip [0..] patterns
        , let preceding = take idx patterns
              matrix = map (: []) preceding
        , not (isUseful env [typeInfo] [pat] matrix)
  ]

-- | Check if a pattern vector is useful given a pattern matrix
--
-- A pattern vector q is useful with respect to matrix P if there exists
-- a value that matches q but doesn't match any row in P.
isUseful :: TypeEnv -> [TypeInfo] -> PatternRow -> PatternMatrix -> Bool
isUseful env types patternVec matrix
  | null patternVec = null matrix  -- Empty pattern matches if matrix is empty
  | null matrix = True             -- Any pattern useful against empty matrix
  | otherwise = case (head patternVec, map head matrix) of
      -- Case 1: First pattern in vector is a constructor
      (PatCon conName subPats, _firstCol) ->
        let specialized = specializeMatrix conName (length subPats) matrix
            newTypes = expandTypes env types conName (length subPats)
            newVec = subPats ++ tail patternVec
        in isUseful env newTypes newVec specialized

      -- Case 2: First pattern is a wildcard or variable
      (pat, firstCol) | isWildcardLike pat ->
        let constructors = case types of
              (t:_) -> typeInfoConstructors t
              [] -> []
        in if any isConstructorPattern firstCol
           then
             -- Check each constructor that appears in the column
             any (\con -> checkConstructor env types patternVec matrix con) constructors
           else
             -- All patterns are wildcards, recurse on rest
             let defaultMat = defaultMatrix matrix
                 restTypes = drop 1 types
                 restVec = tail patternVec
             in isUseful env restTypes restVec defaultMat

      _ -> False

-- | Check usefulness for a specific constructor
checkConstructor :: TypeEnv -> [TypeInfo] -> PatternRow -> PatternMatrix -> ConstructorInfo -> Bool
checkConstructor env types patternVec matrix con =
  let conName = constructorName con
      arity = length (constructorArgs con)
      specialized = specializeMatrix conName arity matrix
      newTypes = expandTypes env types conName arity
      newVec = replicate arity PatWild ++ tail patternVec
  in isUseful env newTypes newVec specialized

-- | Specialize a pattern matrix for a specific constructor
--
-- For each row in the matrix:
-- - If first pattern is the same constructor, replace it with its subpatterns
-- - If first pattern is a wildcard/variable, expand it with wildcards
-- - If first pattern is a different constructor, remove the row
specializeMatrix :: Text -> Int -> PatternMatrix -> PatternMatrix
specializeMatrix conName arity = concatMap specializeRow
  where
    specializeRow [] = []
    specializeRow (p:ps) = case p of
      PatCon name subPats
        | name == conName -> [subPats ++ ps]
        | otherwise -> []  -- Different constructor, row doesn't match
      PatVar _ -> [replicate arity PatWild ++ ps]
      PatWild -> [replicate arity PatWild ++ ps]

-- | Extract the default matrix (rows starting with wildcard/variable)
defaultMatrix :: PatternMatrix -> PatternMatrix
defaultMatrix = concatMap defaultRow
  where
    defaultRow [] = []
    defaultRow (p:ps) = case p of
      PatCon _ _ -> []  -- Constructor rows are not included
      PatVar _ -> [ps]
      PatWild -> [ps]

-- | Expand types for constructor arguments
expandTypes :: TypeEnv -> [TypeInfo] -> Text -> Int -> [TypeInfo]
expandTypes _env types _conName arity =
  -- For now, use a placeholder type for constructor arguments
  let argTypes = replicate arity placeholderType
  in argTypes ++ drop 1 types
  where
    placeholderType = TypeInfo "Any" [ConstructorInfo "_" []]

-- | Check if a pattern is a constructor pattern
isConstructorPattern :: Pattern -> Bool
isConstructorPattern = \case
  PatCon _ _ -> True
  _ -> False

-- | Check if a pattern matches everything (wildcard-like)
isWildcardLike :: Pattern -> Bool
isWildcardLike = \case
  PatWild -> True
  PatVar _ -> True
  _ -> False
