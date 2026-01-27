{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Prelude.Functions
-- Description : Standard function definitions for the Crisp prelude
--
-- Defines the core functions available in the Crisp standard library:
-- identity, composition, Option operations, List operations, etc.
--
-- These functions are defined as Haskell data structures that can be
-- used to generate Crisp function definitions and for type checking.

module Crisp.Prelude.Functions
  ( -- * Function Definitions
    PreludeFunction(..)
  , FunctionBody(..)
    -- * Identity and Composition
  , idFn
  , constFn
  , composeFn
  , flipFn
    -- * Option Operations
  , mapOptionFn
  , unwrapOrFn
  , isNoneFn
  , isSomeFn
  , andThenOptionFn
    -- * Result Operations
  , mapResultFn
  , mapErrorFn
  , isOkFn
  , isErrFn
  , unwrapResultFn
  , andThenResultFn
    -- * List Operations
  , mapFn
  , filterFn
  , foldlFn
  , foldrFn
  , lengthFn
  , appendFn
  , reverseFn
  , headFn
  , tailFn
  , nullFn
  , takeFn
  , dropFn
  , concatFn
    -- * Pair Operations
  , fstFn
  , sndFn
  , swapFn
    -- * Boolean Operations
  , notFn
  , andFn
  , orFn
    -- * All Functions
  , preludeFunctions
    -- * Function Lookup
  , lookupPreludeFunction
  , isPreludeFunction
  ) where

import Crisp.Core.Term (Type(..), Kind(..), EffectRow(..), Term(..), Pattern(..), Case(..))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Function Definitions
--------------------------------------------------------------------------------

-- | A prelude function definition
data PreludeFunction = PreludeFunction
  { fnName       :: !Text              -- ^ Function name
  , fnTypeParams :: ![(Text, Kind)]    -- ^ Type parameters
  , fnParams     :: ![(Text, Type)]    -- ^ Value parameters
  , fnReturnType :: !Type              -- ^ Return type
  , fnEffects    :: !EffectRow         -- ^ Effect row
  , fnBody       :: !FunctionBody      -- ^ Function body
  } deriving stock (Eq, Show)

-- | Function body representation
data FunctionBody
  = BodyTerm !Term                     -- ^ Explicit term
  | BodyBuiltin !Text                  -- ^ Built-in implementation
  | BodyMatch !Text ![(Pattern, Term)] -- ^ Pattern matching on a parameter
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Create a pure function type
pureFn :: Type -> Type -> Type
pureFn from to = TyPi "_" from EffEmpty to

-- | Type variable shorthand
tyVar :: Text -> Int -> Type
tyVar = TyVar

-- | Type constructor shorthand
tyCon :: Text -> [Type] -> Type
tyCon = TyCon

--------------------------------------------------------------------------------
-- Identity and Composition
--------------------------------------------------------------------------------

-- | id : forall A. A -> A
idFn :: PreludeFunction
idFn = PreludeFunction
  { fnName = "id"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams = [("x", tyVar "A" 0)]
  , fnReturnType = tyVar "A" 0
  , fnEffects = EffEmpty
  , fnBody = BodyTerm (TmVar "x" 0)
  }

-- | const : forall A B. A -> B -> A
constFn :: PreludeFunction
constFn = PreludeFunction
  { fnName = "const"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0)]
  , fnParams = [("x", tyVar "A" 0), ("_", tyVar "B" 1)]
  , fnReturnType = tyVar "A" 0
  , fnEffects = EffEmpty
  , fnBody = BodyTerm (TmVar "x" 1)
  }

-- | compose : forall A B C. (B -> C) -> (A -> B) -> (A -> C)
composeFn :: PreludeFunction
composeFn = PreludeFunction
  { fnName = "compose"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0), ("C", KiType 0)]
  , fnParams =
      [ ("f", pureFn (tyVar "B" 1) (tyVar "C" 2))
      , ("g", pureFn (tyVar "A" 0) (tyVar "B" 1))
      , ("x", tyVar "A" 0)
      ]
  , fnReturnType = tyVar "C" 2
  , fnEffects = EffEmpty
  , fnBody = BodyTerm (TmApp (TmVar "f" 2) (TmApp (TmVar "g" 1) (TmVar "x" 0)))
  }

-- | flip : forall A B C. (A -> B -> C) -> (B -> A -> C)
flipFn :: PreludeFunction
flipFn = PreludeFunction
  { fnName = "flip"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0), ("C", KiType 0)]
  , fnParams =
      [ ("f", pureFn (tyVar "A" 0) (pureFn (tyVar "B" 1) (tyVar "C" 2)))
      , ("b", tyVar "B" 1)
      , ("a", tyVar "A" 0)
      ]
  , fnReturnType = tyVar "C" 2
  , fnEffects = EffEmpty
  , fnBody = BodyTerm (TmApp (TmApp (TmVar "f" 2) (TmVar "a" 0)) (TmVar "b" 1))
  }

--------------------------------------------------------------------------------
-- Option Operations
--------------------------------------------------------------------------------

-- | map_option : forall A B. (A -> B) -> Option A -> Option B
mapOptionFn :: PreludeFunction
mapOptionFn = PreludeFunction
  { fnName = "map_option"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0)]
  , fnParams =
      [ ("f", pureFn (tyVar "A" 0) (tyVar "B" 1))
      , ("opt", tyCon "Option" [tyVar "A" 0])
      ]
  , fnReturnType = tyCon "Option" [tyVar "B" 1]
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "opt"
      [ (PatCon "None" [], TmCon "None" [tyVar "B" 1] [])
      , (PatCon "Some" [PatVar "x"], TmCon "Some" [tyVar "B" 1] [TmApp (TmVar "f" 2) (TmVar "x" 0)])
      ]
  }

-- | unwrap_or : forall A. A -> Option A -> A
unwrapOrFn :: PreludeFunction
unwrapOrFn = PreludeFunction
  { fnName = "unwrap_or"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams =
      [ ("default", tyVar "A" 0)
      , ("opt", tyCon "Option" [tyVar "A" 0])
      ]
  , fnReturnType = tyVar "A" 0
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "opt"
      [ (PatCon "None" [], TmVar "default" 1)
      , (PatCon "Some" [PatVar "x"], TmVar "x" 0)
      ]
  }

-- | is_none : forall A. Option A -> Bool
isNoneFn :: PreludeFunction
isNoneFn = PreludeFunction
  { fnName = "is_none"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams = [("opt", tyCon "Option" [tyVar "A" 0])]
  , fnReturnType = tyCon "Bool" []
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "opt"
      [ (PatCon "None" [], TmCon "True" [] [])
      , (PatCon "Some" [PatWild], TmCon "False" [] [])
      ]
  }

-- | is_some : forall A. Option A -> Bool
isSomeFn :: PreludeFunction
isSomeFn = PreludeFunction
  { fnName = "is_some"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams = [("opt", tyCon "Option" [tyVar "A" 0])]
  , fnReturnType = tyCon "Bool" []
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "opt"
      [ (PatCon "None" [], TmCon "False" [] [])
      , (PatCon "Some" [PatWild], TmCon "True" [] [])
      ]
  }

-- | and_then_option : forall A B. (A -> Option B) -> Option A -> Option B
andThenOptionFn :: PreludeFunction
andThenOptionFn = PreludeFunction
  { fnName = "and_then_option"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0)]
  , fnParams =
      [ ("f", pureFn (tyVar "A" 0) (tyCon "Option" [tyVar "B" 1]))
      , ("opt", tyCon "Option" [tyVar "A" 0])
      ]
  , fnReturnType = tyCon "Option" [tyVar "B" 1]
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "opt"
      [ (PatCon "None" [], TmCon "None" [tyVar "B" 1] [])
      , (PatCon "Some" [PatVar "x"], TmApp (TmVar "f" 2) (TmVar "x" 0))
      ]
  }

--------------------------------------------------------------------------------
-- Result Operations
--------------------------------------------------------------------------------

-- | map_result : forall A B E. (A -> B) -> Result A E -> Result B E
mapResultFn :: PreludeFunction
mapResultFn = PreludeFunction
  { fnName = "map_result"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0), ("E", KiType 0)]
  , fnParams =
      [ ("f", pureFn (tyVar "A" 0) (tyVar "B" 1))
      , ("res", tyCon "Result" [tyVar "A" 0, tyVar "E" 2])
      ]
  , fnReturnType = tyCon "Result" [tyVar "B" 1, tyVar "E" 2]
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "res"
      [ (PatCon "Ok" [PatVar "x"], TmCon "Ok" [tyVar "B" 1, tyVar "E" 2] [TmApp (TmVar "f" 2) (TmVar "x" 0)])
      , (PatCon "Err" [PatVar "e"], TmCon "Err" [tyVar "B" 1, tyVar "E" 2] [TmVar "e" 0])
      ]
  }

-- | map_error : forall A E F. (E -> F) -> Result A E -> Result A F
mapErrorFn :: PreludeFunction
mapErrorFn = PreludeFunction
  { fnName = "map_error"
  , fnTypeParams = [("A", KiType 0), ("E", KiType 0), ("F", KiType 0)]
  , fnParams =
      [ ("f", pureFn (tyVar "E" 1) (tyVar "F" 2))
      , ("res", tyCon "Result" [tyVar "A" 0, tyVar "E" 1])
      ]
  , fnReturnType = tyCon "Result" [tyVar "A" 0, tyVar "F" 2]
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "res"
      [ (PatCon "Ok" [PatVar "x"], TmCon "Ok" [tyVar "A" 0, tyVar "F" 2] [TmVar "x" 0])
      , (PatCon "Err" [PatVar "e"], TmCon "Err" [tyVar "A" 0, tyVar "F" 2] [TmApp (TmVar "f" 2) (TmVar "e" 0)])
      ]
  }

-- | is_ok : forall A E. Result A E -> Bool
isOkFn :: PreludeFunction
isOkFn = PreludeFunction
  { fnName = "is_ok"
  , fnTypeParams = [("A", KiType 0), ("E", KiType 0)]
  , fnParams = [("res", tyCon "Result" [tyVar "A" 0, tyVar "E" 1])]
  , fnReturnType = tyCon "Bool" []
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "res"
      [ (PatCon "Ok" [PatWild], TmCon "True" [] [])
      , (PatCon "Err" [PatWild], TmCon "False" [] [])
      ]
  }

-- | is_err : forall A E. Result A E -> Bool
isErrFn :: PreludeFunction
isErrFn = PreludeFunction
  { fnName = "is_err"
  , fnTypeParams = [("A", KiType 0), ("E", KiType 0)]
  , fnParams = [("res", tyCon "Result" [tyVar "A" 0, tyVar "E" 1])]
  , fnReturnType = tyCon "Bool" []
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "res"
      [ (PatCon "Ok" [PatWild], TmCon "False" [] [])
      , (PatCon "Err" [PatWild], TmCon "True" [] [])
      ]
  }

-- | unwrap_result : forall A E. Result A E -> A (panics on Err)
unwrapResultFn :: PreludeFunction
unwrapResultFn = PreludeFunction
  { fnName = "unwrap_result"
  , fnTypeParams = [("A", KiType 0), ("E", KiType 0)]
  , fnParams = [("res", tyCon "Result" [tyVar "A" 0, tyVar "E" 1])]
  , fnReturnType = tyVar "A" 0
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "unwrap_result"  -- Requires runtime panic
  }

-- | and_then_result : forall A B E. (A -> Result B E) -> Result A E -> Result B E
andThenResultFn :: PreludeFunction
andThenResultFn = PreludeFunction
  { fnName = "and_then_result"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0), ("E", KiType 0)]
  , fnParams =
      [ ("f", pureFn (tyVar "A" 0) (tyCon "Result" [tyVar "B" 1, tyVar "E" 2]))
      , ("res", tyCon "Result" [tyVar "A" 0, tyVar "E" 2])
      ]
  , fnReturnType = tyCon "Result" [tyVar "B" 1, tyVar "E" 2]
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "res"
      [ (PatCon "Ok" [PatVar "x"], TmApp (TmVar "f" 2) (TmVar "x" 0))
      , (PatCon "Err" [PatVar "e"], TmCon "Err" [tyVar "B" 1, tyVar "E" 2] [TmVar "e" 0])
      ]
  }

--------------------------------------------------------------------------------
-- List Operations
--------------------------------------------------------------------------------

-- | map : forall A B. (A -> B) -> List A -> List B
mapFn :: PreludeFunction
mapFn = PreludeFunction
  { fnName = "map"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0)]
  , fnParams =
      [ ("f", pureFn (tyVar "A" 0) (tyVar "B" 1))
      , ("xs", tyCon "List" [tyVar "A" 0])
      ]
  , fnReturnType = tyCon "List" [tyVar "B" 1]
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "map"  -- Recursive, implemented as builtin
  }

-- | filter : forall A. (A -> Bool) -> List A -> List A
filterFn :: PreludeFunction
filterFn = PreludeFunction
  { fnName = "filter"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams =
      [ ("p", pureFn (tyVar "A" 0) (tyCon "Bool" []))
      , ("xs", tyCon "List" [tyVar "A" 0])
      ]
  , fnReturnType = tyCon "List" [tyVar "A" 0]
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "filter"
  }

-- | foldl : forall A B. (B -> A -> B) -> B -> List A -> B
foldlFn :: PreludeFunction
foldlFn = PreludeFunction
  { fnName = "foldl"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0)]
  , fnParams =
      [ ("f", pureFn (tyVar "B" 1) (pureFn (tyVar "A" 0) (tyVar "B" 1)))
      , ("init", tyVar "B" 1)
      , ("xs", tyCon "List" [tyVar "A" 0])
      ]
  , fnReturnType = tyVar "B" 1
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "foldl"
  }

-- | foldr : forall A B. (A -> B -> B) -> B -> List A -> B
foldrFn :: PreludeFunction
foldrFn = PreludeFunction
  { fnName = "foldr"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0)]
  , fnParams =
      [ ("f", pureFn (tyVar "A" 0) (pureFn (tyVar "B" 1) (tyVar "B" 1)))
      , ("init", tyVar "B" 1)
      , ("xs", tyCon "List" [tyVar "A" 0])
      ]
  , fnReturnType = tyVar "B" 1
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "foldr"
  }

-- | length : forall A. List A -> Int
lengthFn :: PreludeFunction
lengthFn = PreludeFunction
  { fnName = "length"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams = [("xs", tyCon "List" [tyVar "A" 0])]
  , fnReturnType = tyCon "Int" []
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "length"
  }

-- | append : forall A. List A -> List A -> List A
appendFn :: PreludeFunction
appendFn = PreludeFunction
  { fnName = "append"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams =
      [ ("xs", tyCon "List" [tyVar "A" 0])
      , ("ys", tyCon "List" [tyVar "A" 0])
      ]
  , fnReturnType = tyCon "List" [tyVar "A" 0]
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "append"
  }

-- | reverse : forall A. List A -> List A
reverseFn :: PreludeFunction
reverseFn = PreludeFunction
  { fnName = "reverse"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams = [("xs", tyCon "List" [tyVar "A" 0])]
  , fnReturnType = tyCon "List" [tyVar "A" 0]
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "reverse"
  }

-- | head : forall A. List A -> Option A
headFn :: PreludeFunction
headFn = PreludeFunction
  { fnName = "head"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams = [("xs", tyCon "List" [tyVar "A" 0])]
  , fnReturnType = tyCon "Option" [tyVar "A" 0]
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "xs"
      [ (PatCon "Nil" [], TmCon "None" [tyVar "A" 0] [])
      , (PatCon "Cons" [PatVar "x", PatWild], TmCon "Some" [tyVar "A" 0] [TmVar "x" 0])
      ]
  }

-- | tail : forall A. List A -> Option (List A)
tailFn :: PreludeFunction
tailFn = PreludeFunction
  { fnName = "tail"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams = [("xs", tyCon "List" [tyVar "A" 0])]
  , fnReturnType = tyCon "Option" [tyCon "List" [tyVar "A" 0]]
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "xs"
      [ (PatCon "Nil" [], TmCon "None" [tyCon "List" [tyVar "A" 0]] [])
      , (PatCon "Cons" [PatWild, PatVar "rest"], TmCon "Some" [tyCon "List" [tyVar "A" 0]] [TmVar "rest" 0])
      ]
  }

-- | null : forall A. List A -> Bool
nullFn :: PreludeFunction
nullFn = PreludeFunction
  { fnName = "null"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams = [("xs", tyCon "List" [tyVar "A" 0])]
  , fnReturnType = tyCon "Bool" []
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "xs"
      [ (PatCon "Nil" [], TmCon "True" [] [])
      , (PatCon "Cons" [PatWild, PatWild], TmCon "False" [] [])
      ]
  }

-- | take : forall A. Int -> List A -> List A
takeFn :: PreludeFunction
takeFn = PreludeFunction
  { fnName = "take"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams =
      [ ("n", tyCon "Int" [])
      , ("xs", tyCon "List" [tyVar "A" 0])
      ]
  , fnReturnType = tyCon "List" [tyVar "A" 0]
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "take"
  }

-- | drop : forall A. Int -> List A -> List A
dropFn :: PreludeFunction
dropFn = PreludeFunction
  { fnName = "drop"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams =
      [ ("n", tyCon "Int" [])
      , ("xs", tyCon "List" [tyVar "A" 0])
      ]
  , fnReturnType = tyCon "List" [tyVar "A" 0]
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "drop"
  }

-- | concat : forall A. List (List A) -> List A
concatFn :: PreludeFunction
concatFn = PreludeFunction
  { fnName = "concat"
  , fnTypeParams = [("A", KiType 0)]
  , fnParams = [("xss", tyCon "List" [tyCon "List" [tyVar "A" 0]])]
  , fnReturnType = tyCon "List" [tyVar "A" 0]
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "concat"
  }

--------------------------------------------------------------------------------
-- Pair Operations
--------------------------------------------------------------------------------

-- | fst : forall A B. Pair A B -> A
fstFn :: PreludeFunction
fstFn = PreludeFunction
  { fnName = "fst"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0)]
  , fnParams = [("p", tyCon "Pair" [tyVar "A" 0, tyVar "B" 1])]
  , fnReturnType = tyVar "A" 0
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "p"
      [ (PatCon "Pair" [PatVar "a", PatWild], TmVar "a" 0)
      ]
  }

-- | snd : forall A B. Pair A B -> B
sndFn :: PreludeFunction
sndFn = PreludeFunction
  { fnName = "snd"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0)]
  , fnParams = [("p", tyCon "Pair" [tyVar "A" 0, tyVar "B" 1])]
  , fnReturnType = tyVar "B" 1
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "p"
      [ (PatCon "Pair" [PatWild, PatVar "b"], TmVar "b" 0)
      ]
  }

-- | swap : forall A B. Pair A B -> Pair B A
swapFn :: PreludeFunction
swapFn = PreludeFunction
  { fnName = "swap"
  , fnTypeParams = [("A", KiType 0), ("B", KiType 0)]
  , fnParams = [("p", tyCon "Pair" [tyVar "A" 0, tyVar "B" 1])]
  , fnReturnType = tyCon "Pair" [tyVar "B" 1, tyVar "A" 0]
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "p"
      [ (PatCon "Pair" [PatVar "a", PatVar "b"], TmCon "Pair" [tyVar "B" 1, tyVar "A" 0] [TmVar "b" 0, TmVar "a" 1])
      ]
  }

--------------------------------------------------------------------------------
-- Boolean Operations
--------------------------------------------------------------------------------

-- | not : Bool -> Bool
notFn :: PreludeFunction
notFn = PreludeFunction
  { fnName = "not"
  , fnTypeParams = []
  , fnParams = [("b", tyCon "Bool" [])]
  , fnReturnType = tyCon "Bool" []
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "b"
      [ (PatCon "True" [], TmCon "False" [] [])
      , (PatCon "False" [], TmCon "True" [] [])
      ]
  }

-- | and : Bool -> Bool -> Bool
andFn :: PreludeFunction
andFn = PreludeFunction
  { fnName = "and"
  , fnTypeParams = []
  , fnParams = [("a", tyCon "Bool" []), ("b", tyCon "Bool" [])]
  , fnReturnType = tyCon "Bool" []
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "a"
      [ (PatCon "True" [], TmVar "b" 0)
      , (PatCon "False" [], TmCon "False" [] [])
      ]
  }

-- | or : Bool -> Bool -> Bool
orFn :: PreludeFunction
orFn = PreludeFunction
  { fnName = "or"
  , fnTypeParams = []
  , fnParams = [("a", tyCon "Bool" []), ("b", tyCon "Bool" [])]
  , fnReturnType = tyCon "Bool" []
  , fnEffects = EffEmpty
  , fnBody = BodyMatch "a"
      [ (PatCon "True" [], TmCon "True" [] [])
      , (PatCon "False" [], TmVar "b" 0)
      ]
  }

--------------------------------------------------------------------------------
-- All Functions
--------------------------------------------------------------------------------

-- | All standard prelude functions
preludeFunctions :: [PreludeFunction]
preludeFunctions =
  -- Identity and composition
  [ idFn, constFn, composeFn, flipFn
  -- Option operations
  , mapOptionFn, unwrapOrFn, isNoneFn, isSomeFn, andThenOptionFn
  -- Result operations
  , mapResultFn, mapErrorFn, isOkFn, isErrFn, unwrapResultFn, andThenResultFn
  -- List operations
  , mapFn, filterFn, foldlFn, foldrFn, lengthFn, appendFn, reverseFn
  , headFn, tailFn, nullFn, takeFn, dropFn, concatFn
  -- Pair operations
  , fstFn, sndFn, swapFn
  -- Boolean operations
  , notFn, andFn, orFn
  ]

--------------------------------------------------------------------------------
-- Function Lookup
--------------------------------------------------------------------------------

-- | Map of function names to definitions
preludeFunctionMap :: Map Text PreludeFunction
preludeFunctionMap = Map.fromList [(fnName f, f) | f <- preludeFunctions]

-- | Look up a prelude function by name
lookupPreludeFunction :: Text -> Maybe PreludeFunction
lookupPreludeFunction name = Map.lookup name preludeFunctionMap

-- | Check if a name is a prelude function
isPreludeFunction :: Text -> Bool
isPreludeFunction name = Map.member name preludeFunctionMap
