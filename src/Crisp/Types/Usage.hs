{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.Usage
-- Description : Usage counting for linear types
--
-- Provides usage counting and linearity modes for tracking how many
-- times values are used in expressions.

module Crisp.Types.Usage
  ( -- * Usage Types
    Usage(..)
  , usageCount
  , isZero
  , isOne
  , isMany
    -- * Usage Operations
  , addUsage
  , altUsage
  , scaleUsage
  , isInconsistentUsage
    -- * Linearity Modes
  , LinearityMode(..)
  , isUniqueMode
  , isBorrowedMode
  , isSharedMode
  , isUnrestrictedMode
  , requiresExactlyOnce
  , allowsMultipleUse
  ) where

import GHC.Generics (Generic)

-- =============================================================================
-- Usage Types
-- =============================================================================

-- | Usage count for a variable
data Usage
  = Zero                             -- ^ Not used at all
  | One                              -- ^ Used exactly once
  | Many                             -- ^ Used more than once
  | Inconsistent                     -- ^ Used in some branches but not others
  deriving stock (Eq, Show, Generic)

-- | Get a numeric representation of usage
usageCount :: Usage -> Int
usageCount Zero = 0
usageCount One = 1
usageCount Many = 2  -- Represents "2 or more"
usageCount Inconsistent = -1  -- Invalid

-- | Check if usage is zero
isZero :: Usage -> Bool
isZero Zero = True
isZero _ = False

-- | Check if usage is exactly one
isOne :: Usage -> Bool
isOne One = True
isOne _ = False

-- | Check if usage is many
isMany :: Usage -> Bool
isMany Many = True
isMany _ = False

-- | Check if usage is inconsistent across branches
isInconsistentUsage :: Usage -> Bool
isInconsistentUsage Inconsistent = True
isInconsistentUsage _ = False

-- =============================================================================
-- Usage Operations
-- =============================================================================

-- | Add usage counts (for sequential execution)
-- Both code paths execute, so usages add up
addUsage :: Usage -> Usage -> Usage
addUsage Zero u = u
addUsage u Zero = u
addUsage One One = Many
addUsage One Many = Many
addUsage Many One = Many
addUsage Many Many = Many
addUsage Inconsistent _ = Inconsistent
addUsage _ Inconsistent = Inconsistent

-- | Alternative usage (for conditional execution)
-- Only one branch executes, so we need consistency
altUsage :: Usage -> Usage -> Usage
altUsage Zero Zero = Zero
altUsage One One = One
altUsage Many Many = Many
-- If usage differs between branches, it's inconsistent for linear vars
altUsage Zero One = Inconsistent
altUsage One Zero = Inconsistent
altUsage Zero Many = Inconsistent
altUsage Many Zero = Inconsistent
altUsage One Many = Many  -- Safe: at least one use in each branch
altUsage Many One = Many  -- Safe: at least one use in each branch
altUsage Inconsistent _ = Inconsistent
altUsage _ Inconsistent = Inconsistent

-- | Scale usage by a factor (for repeated execution)
scaleUsage :: Usage -> Int -> Usage
scaleUsage _ 0 = Zero
scaleUsage Zero _ = Zero
scaleUsage One 1 = One
scaleUsage One n
  | n > 1 = Many
  | otherwise = Zero
scaleUsage Many _ = Many
scaleUsage Inconsistent _ = Inconsistent

-- =============================================================================
-- Linearity Modes
-- =============================================================================

-- | Linearity mode for a binding
data LinearityMode
  = Unique                           -- ^ Must be used exactly once
  | Borrowed                         -- ^ Can be read but not consumed
  | Shared                           -- ^ Can be shared among multiple readers
  | Unrestricted                     -- ^ No linearity restrictions
  deriving stock (Eq, Show, Generic)

-- | Check if mode is Unique
isUniqueMode :: LinearityMode -> Bool
isUniqueMode Unique = True
isUniqueMode _ = False

-- | Check if mode is Borrowed
isBorrowedMode :: LinearityMode -> Bool
isBorrowedMode Borrowed = True
isBorrowedMode _ = False

-- | Check if mode is Shared
isSharedMode :: LinearityMode -> Bool
isSharedMode Shared = True
isSharedMode _ = False

-- | Check if mode is Unrestricted
isUnrestrictedMode :: LinearityMode -> Bool
isUnrestrictedMode Unrestricted = True
isUnrestrictedMode _ = False

-- | Check if mode requires exactly one use
requiresExactlyOnce :: LinearityMode -> Bool
requiresExactlyOnce Unique = True
requiresExactlyOnce _ = False

-- | Check if mode allows multiple uses
allowsMultipleUse :: LinearityMode -> Bool
allowsMultipleUse Borrowed = True
allowsMultipleUse Shared = True
allowsMultipleUse Unrestricted = True
allowsMultipleUse Unique = False
