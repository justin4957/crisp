{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Effects.Row
-- Description : Effect Row Operations
--
-- Provides operations on effect rows for the algebraic effect system.

module Crisp.Effects.Row
  ( -- * Predicates
    isPure
  , containsEffect
  , isSubset
    -- * Operations
  , concreteEffects
  , effectNames
  , union
  , remove
  , normalize
    -- * Construction
  , fromNames
  , singleton
  , singletonWithAuthority
    -- * Rendering
  , effectRowToText
    -- * Authority
  , authorities
  , authorityMismatches
  ) where

import Crisp.Core.Term

import Data.List (nub, sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T

-- | Check if an effect row is pure (no effects)
isPure :: EffectRow -> Bool
isPure EffEmpty = True
isPure (EffSet []) = True
isPure (EffSet _) = False
isPure (EffVar _ _) = False
isPure (EffUnion a b) = isPure a && isPure b

-- | Get all concrete effects from an effect row
concreteEffects :: EffectRow -> [Effect]
concreteEffects EffEmpty = []
concreteEffects (EffSet effs) = effs
concreteEffects (EffVar _ _) = []
concreteEffects (EffUnion a b) = concreteEffects a ++ concreteEffects b

-- | Get all effect names from an effect row
effectNames :: EffectRow -> [Text]
effectNames = nub . map effectName . concreteEffects

-- | Check if an effect row contains a specific effect
containsEffect :: Text -> EffectRow -> Bool
containsEffect name row = name `elem` effectNames row

-- | Create an effect row from a list of effect names
fromNames :: [Text] -> EffectRow
fromNames [] = EffEmpty
fromNames names = EffSet $ map (`Effect` Nothing) names

-- | Create an effect row with a single effect
singleton :: Text -> EffectRow
singleton name = EffSet [Effect name Nothing]

-- | Create an effect row with a single effect and authority
singletonWithAuthority :: Text -> Text -> EffectRow
singletonWithAuthority name auth = EffSet [Effect name (Just auth)]

-- | Union two effect rows
union :: EffectRow -> EffectRow -> EffectRow
union EffEmpty other = other
union other EffEmpty = other
union (EffSet a) (EffSet b) = EffSet (mergeEffects a b)
union a b = EffUnion a b

-- | Merge two effect lists, deduplicating by name
mergeEffects :: [Effect] -> [Effect] -> [Effect]
mergeEffects a b = nub (a ++ b)

-- | Remove an effect from an effect row
remove :: Text -> EffectRow -> EffectRow
remove _ EffEmpty = EffEmpty
remove name (EffSet effs) =
  case filter (\e -> effectName e /= name) effs of
    [] -> EffEmpty
    filtered -> EffSet filtered
remove _ row@(EffVar _ _) = row
remove name (EffUnion a b) = union (remove name a) (remove name b)

-- | Check if effect row A is a subset of effect row B
isSubset :: EffectRow -> EffectRow -> Bool
isSubset a b =
  let namesA = effectNames a
      namesB = effectNames b
  in all (`elem` namesB) namesA

-- | Pretty-print an effect row
effectRowToText :: EffectRow -> Text
effectRowToText EffEmpty = "Pure"
effectRowToText (EffSet []) = "Pure"
effectRowToText (EffSet effs) = T.intercalate ", " (map effectToText effs)
effectRowToText (EffVar name _) = name
effectRowToText (EffUnion a b) = effectRowToText a <> " | " <> effectRowToText b

effectToText :: Effect -> Text
effectToText (Effect name Nothing) = name
effectToText (Effect name (Just auth)) = name <> " @ " <> auth

-- | Extract authority annotations from an effect row
authorities :: EffectRow -> [(Text, Text)]
authorities = foldr extractAuth [] . concreteEffects
  where
    extractAuth (Effect name (Just auth)) acc = (name, auth) : acc
    extractAuth _ acc = acc

-- | Check for authority mismatches between two effect rows
authorityMismatches :: EffectRow -> EffectRow -> [(Text, Maybe Text, Maybe Text)]
authorityMismatches a b =
  let effsA = concreteEffects a
      effsB = concreteEffects b
  in [ (effectName effA, effectAuthority effA, effectAuthority effB)
     | effA <- effsA
     , effB <- effsB
     , effectName effA == effectName effB
     , effectAuthority effA /= effectAuthority effB
     ]

-- | Normalize an effect row (flatten unions, deduplicate)
normalize :: EffectRow -> EffectRow
normalize EffEmpty = EffEmpty
normalize (EffSet effs) =
  case deduplicateEffects effs of
    [] -> EffEmpty
    unique -> EffSet unique
normalize row@(EffVar _ _) = row
normalize (EffUnion a b) =
  let effsA = concreteEffects (normalize a)
      effsB = concreteEffects (normalize b)
      merged = mergeEffects effsA effsB
  in case merged of
       [] -> EffEmpty
       _  -> EffSet merged

-- | Deduplicate effects by name
deduplicateEffects :: [Effect] -> [Effect]
deduplicateEffects = nub
