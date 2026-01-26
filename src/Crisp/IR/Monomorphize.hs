{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.IR.Monomorphize
-- Description : Monomorphization pass for LLIR generation
--
-- Implements monomorphization to convert polymorphic typed IR into
-- monomorphic code suitable for WebAssembly:
-- - Instantiates all polymorphic functions at concrete types
-- - Handles recursive polymorphic functions with cycle detection
-- - Generates type-specialized function names
-- - Eliminates dead code from unused specializations
--
-- WebAssembly has no parametric polymorphism, so all type parameters
-- must be resolved to concrete types before code generation.

module Crisp.IR.Monomorphize
  ( -- * Monomorphization Input
    MonoEnv(..)
  , PolyFunc(..)
  , MonoBody(..)
  , CallSite(..)
    -- * Monomorphization Output
  , MonoResult(..)
  , monoResultFuncs
  , monoResultHasCycleWarning
    -- * Monomorphization
  , monomorphize
  , monomorphizeWithDepth
    -- * Specialized Naming
  , specializedName
    -- * Dead Code Elimination
  , eliminateDeadCode
  ) where

import Crisp.IR.LLIR
import Crisp.Core.Term (Type(..), Kind(..), EffectRow(..))

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub, foldl')
import Data.Maybe (mapMaybe)

--------------------------------------------------------------------------------
-- Monomorphization Input
--------------------------------------------------------------------------------

-- | Environment for monomorphization
data MonoEnv = MonoEnv
  { monoEnvFuncs     :: ![PolyFunc]   -- ^ Polymorphic functions to monomorphize
  , monoEnvCallSites :: ![CallSite]   -- ^ Call sites with concrete type arguments
  }
  deriving stock (Eq, Show)

-- | A potentially polymorphic function
data PolyFunc = PolyFunc
  { polyFuncName       :: !Text
  , polyFuncTypeParams :: ![(Text, Kind)]
  , polyFuncParams     :: ![(Text, Type)]
  , polyFuncReturn     :: !Type
  , polyFuncBody       :: !MonoBody
  }
  deriving stock (Eq, Show)

-- | Simplified function body for monomorphization analysis
data MonoBody
  = MonoBody ![LlirInstr]
    -- ^ Concrete instructions
  | MonoBodyWithCalls ![(Text, [Type])]
    -- ^ Body with calls to other functions (name, type args)
  deriving stock (Eq, Show)

-- | A call site with concrete type arguments
data CallSite = CallSite
  { callSiteName :: !Text
  , callSiteTypeArgs :: ![Type]
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Monomorphization Output
--------------------------------------------------------------------------------

-- | Result of monomorphization
data MonoResult = MonoResult
  { monoResultFuncs'        :: ![LlirFunc]
  , monoResultCycleWarning :: !Bool
  }
  deriving stock (Eq, Show)

-- | Get the monomorphized functions
monoResultFuncs :: MonoResult -> [LlirFunc]
monoResultFuncs = monoResultFuncs'

-- | Check if a cycle warning was generated
monoResultHasCycleWarning :: MonoResult -> Bool
monoResultHasCycleWarning = monoResultCycleWarning

--------------------------------------------------------------------------------
-- Monomorphization
--------------------------------------------------------------------------------

-- | Default maximum instantiation depth
defaultMaxDepth :: Int
defaultMaxDepth = 10

-- | Monomorphize all polymorphic functions
monomorphize :: MonoEnv -> MonoResult
monomorphize = monomorphizeWithDepth defaultMaxDepth

-- | Monomorphize with a specific depth limit
monomorphizeWithDepth :: Int -> MonoEnv -> MonoResult
monomorphizeWithDepth maxDepth env =
  let funcMap = Map.fromList [(polyFuncName f, f) | f <- monoEnvFuncs env]
      -- Include monomorphic functions directly (they don't need call sites)
      monoFuncs = [f | f <- monoEnvFuncs env, null (polyFuncTypeParams f)]
      monoSpecs = [Specialization (polyFuncName f) [] | f <- monoFuncs]
      -- Add call sites for polymorphic functions
      initialCallSites = monoEnvCallSites env
      (polySpecs, hasCycle) = collectSpecializations maxDepth funcMap initialCallSites
      -- Combine: monomorphic first (in order), then polymorphic specializations
      allSpecs = monoSpecs ++ filter (not . null . specTypeArgs) (reverse polySpecs)
      -- Remove duplicates (monomorphic functions might also have call sites)
      seenKeys = Set.empty
      (finalSpecs, _) = foldl' addIfNew ([], seenKeys) allSpecs
      llirFuncs = map (specToLlir funcMap) (reverse finalSpecs)
  in MonoResult llirFuncs hasCycle
  where
    addIfNew (acc, seen) spec
      | specKey spec `Set.member` seen = (acc, seen)
      | otherwise = (spec : acc, Set.insert (specKey spec) seen)

-- | A specialization request
data Specialization = Specialization
  { specFuncName :: !Text
  , specTypeArgs :: ![Type]
  }
  deriving stock (Eq, Show)

-- | Specialization key for Set membership (using specialized name as key)
specKey :: Specialization -> Text
specKey spec = specializedName (specFuncName spec) (specTypeArgs spec)

-- | Collect all required specializations
collectSpecializations
  :: Int
  -> Map Text PolyFunc
  -> [CallSite]
  -> ([Specialization], Bool)
collectSpecializations maxDepth funcMap initialCallSites =
  go maxDepth Set.empty (map toSpec initialCallSites) [] False
  where
    toSpec (CallSite name args) = Specialization name args

    go :: Int -> Set Text -> [Specialization] -> [Specialization] -> Bool -> ([Specialization], Bool)
    go 0 _ pending collected _ = (collected ++ pending, True)  -- Hit depth limit
    go _ _ [] collected hasCycle = (collected, hasCycle)
    go depth seen (spec:rest) collected hasCycle
      | specKey spec `Set.member` seen =
          go depth seen rest collected hasCycle
      | otherwise =
          let newCalls = getCallsFromSpec funcMap spec
              newSpecs = filter (\s -> specKey s `Set.notMember` seen) newCalls
              newSeen = Set.insert (specKey spec) seen
          in go (depth - 1) newSeen (rest ++ newSpecs) (spec : collected) hasCycle

-- | Get call sites from a specialization
getCallsFromSpec :: Map Text PolyFunc -> Specialization -> [Specialization]
getCallsFromSpec funcMap spec =
  case Map.lookup (specFuncName spec) funcMap of
    Nothing -> []
    Just func ->
      let subst = buildTypeSubst (polyFuncTypeParams func) (specTypeArgs spec)
      in case polyFuncBody func of
           MonoBody _ -> []
           MonoBodyWithCalls calls ->
             [Specialization name (map (substituteType subst) args) | (name, args) <- calls]

-- | Build type substitution from type params and args
buildTypeSubst :: [(Text, Kind)] -> [Type] -> Map Text Type
buildTypeSubst params args =
  Map.fromList (zip (map fst params) args)

-- | Apply type substitution
substituteType :: Map Text Type -> Type -> Type
substituteType subst = go
  where
    go = \case
      TyVar name _ ->
        Map.findWithDefault (TyVar name 0) name subst
      TyCon name args ->
        TyCon name (map go args)
      TyPi x a eff b ->
        TyPi x (go a) eff (go b)
      TyForall x k body ->
        TyForall x k (go body)
      TyForallDep x a body ->
        TyForallDep x (go a) (go body)
      TySigma x a b ->
        TySigma x (go a) (go b)
      TyLazy t -> TyLazy (go t)
      TyLinear t -> TyLinear (go t)
      TyRef t -> TyRef (go t)
      TyRefMut t -> TyRefMut (go t)
      TyAdd t1 t2 -> TyAdd (go t1) (go t2)
      TyEffect name t -> TyEffect name (go t)
      t -> t  -- TyUniverse, TyProp, TyNatLit

-- | Convert a specialization to LLIR function
specToLlir :: Map Text PolyFunc -> Specialization -> LlirFunc
specToLlir funcMap spec =
  case Map.lookup (specFuncName spec) funcMap of
    Nothing ->
      -- Unknown function - create a stub
      LlirFunc
        { llirFuncName = specializedName (specFuncName spec) (specTypeArgs spec)
        , llirFuncParams = []
        , llirFuncReturn = LlirUnit
        , llirFuncLocals = []
        , llirFuncBody = [LlirUnreachable]
        }
    Just func ->
      let subst = buildTypeSubst (polyFuncTypeParams func) (specTypeArgs spec)
          params = [(n, toLlirType (substituteType subst t)) | (n, t) <- polyFuncParams func]
          ret = toLlirType (substituteType subst (polyFuncReturn func))
          body = case polyFuncBody func of
            MonoBody instrs -> instrs
            MonoBodyWithCalls _ -> []  -- Simplified - real impl would transform body
      in LlirFunc
           { llirFuncName = specializedName (specFuncName spec) (specTypeArgs spec)
           , llirFuncParams = params
           , llirFuncReturn = ret
           , llirFuncLocals = []
           , llirFuncBody = body
           }

--------------------------------------------------------------------------------
-- Specialized Naming
--------------------------------------------------------------------------------

-- | Generate a specialized function name
specializedName :: Text -> [Type] -> Text
specializedName baseName [] = baseName
specializedName baseName typeArgs =
  baseName <> "$" <> T.intercalate "$" (map typeToNamePart typeArgs)

-- | Convert a type to a name component
typeToNamePart :: Type -> Text
typeToNamePart = \case
  TyCon name [] -> name
  TyCon name args -> name <> "_" <> T.intercalate "_" (map typeToNamePart args)
  TyVar name _ -> name
  TyPi _ _ _ _ -> "Fn"
  TyForall _ _ _ -> "Poly"
  TyForallDep _ _ _ -> "Poly"
  TySigma _ _ _ -> "Sigma"
  TyLazy t -> "Lazy_" <> typeToNamePart t
  TyLinear t -> "Lin_" <> typeToNamePart t
  TyRef t -> "Ref_" <> typeToNamePart t
  TyRefMut t -> "RefMut_" <> typeToNamePart t
  TyUniverse n -> "Type" <> T.pack (show n)
  TyProp -> "Prop"
  TyNatLit n -> "N" <> T.pack (show n)
  TyAdd t1 t2 -> typeToNamePart t1 <> "Plus" <> typeToNamePart t2
  TyEffect name _ -> "Eff_" <> name

--------------------------------------------------------------------------------
-- Dead Code Elimination
--------------------------------------------------------------------------------

-- | Eliminate unused functions
eliminateDeadCode :: [LlirFunc] -> Set Text -> [LlirFunc]
eliminateDeadCode funcs exports =
  let funcMap = Map.fromList [(llirFuncName f, f) | f <- funcs]
      reachable = findReachable funcMap exports
  in filter (\f -> llirFuncName f `Set.member` reachable) funcs

-- | Find all reachable functions from exports
findReachable :: Map Text LlirFunc -> Set Text -> Set Text
findReachable funcMap exports =
  go exports (Set.toList exports)
  where
    go reached [] = reached
    go reached (name:rest)
      | Just func <- Map.lookup name funcMap =
          let calls = extractCalls (llirFuncBody func)
              newCalls = filter (`Set.notMember` reached) calls
              newReached = foldl' (flip Set.insert) reached newCalls
          in go newReached (rest ++ newCalls)
      | otherwise =
          go reached rest

-- | Extract function calls from instructions
extractCalls :: [LlirInstr] -> [Text]
extractCalls = concatMap extractCall
  where
    extractCall = \case
      LlirCall name _ -> [name]
      LlirBlock _ body -> extractCalls body
      LlirLoop _ body -> extractCalls body
      LlirIf _ thenBranch elseBranch -> extractCalls thenBranch ++ extractCalls elseBranch
      _ -> []
