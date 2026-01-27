{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.IR.ENIR
-- Description : Effect-Normalized Intermediate Representation
--
-- After proof erasure and CPS transformation of effects.
-- This IR has explicit continuation passing for effect operations.

module Crisp.IR.ENIR
  ( -- * Terms
    ENIRTerm(..)
  , ENIRType(..)
  , ENIRValue(..)
    -- * Handlers
  , ENIRHandler(..)
  , ENIROpHandler(..)
  , ENIRReturnHandler(..)
    -- * Continuation
  , Continuation(..)
    -- * Transformation
  , toENIR
  , transformHandler
  , transformOpHandler
    -- * ANF Transformation
  , normalizeToValue
  , NormalizeResult(..)
  ) where

import Crisp.Core.Term (Term(..), Type(..), Kind(..), EffectRow(..), Effect(..), Pattern(..), Case(..), Handler(..), OpHandler(..), ReturnHandler(..), ExternalBinding(..))
import qualified Crisp.Core.Term as Core

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State.Strict (State, runState, get, put)

-- | ENIR terms (continuation-passing style)
data ENIRTerm
  = ENIRVar !Text !Int
  | ENIRLam !Text !ENIRType !ENIRTerm
  | ENIRApp !ENIRTerm !ENIRValue
  | ENIRLet !Text !ENIRType !ENIRTerm !ENIRTerm
  | ENIRCon !Text ![ENIRValue]
  | ENIRMatch !ENIRValue ![(Pattern, ENIRTerm)]
  | ENIRCall !Text !Text !ENIRValue !Continuation  -- ^ Effect call with continuation
  | ENIRHandle !ENIRHandler !ENIRTerm              -- ^ Effect handler around a body
  | ENIRReturn !ENIRValue
  deriving stock (Eq, Show)

-- | ENIR values (fully evaluated)
data ENIRValue
  = ENIRVVar !Text !Int
  | ENIRVCon !Text ![ENIRValue]
  | ENIRVLam !Text !ENIRType !ENIRTerm
  deriving stock (Eq, Show)

-- | ENIR types (no Prop types, no dependent types at runtime)
data ENIRType
  = ENIRTyCon !Text ![ENIRType]
  | ENIRTyFn !ENIRType !ENIRType
  | ENIRTyLazy !ENIRType
  deriving stock (Eq, Show)

-- | A continuation for effect handling
data Continuation = Continuation
  { contParam :: !Text
  , contBody  :: !ENIRTerm
  } deriving stock (Eq, Show)

-- | An effect handler in ENIR
-- Handlers intercept effect operations and provide implementations
data ENIRHandler = ENIRHandler
  { enirHandlerEffect     :: !Text                -- ^ Effect being handled (e.g., "State")
  , enirHandlerOps        :: ![ENIROpHandler]     -- ^ Operation handlers
  , enirHandlerReturn     :: !ENIRReturnHandler   -- ^ Return clause
  } deriving stock (Eq, Show)

-- | Handler for a single effect operation
-- The resumption is represented as a continuation parameter
data ENIROpHandler = ENIROpHandler
  { enirOpName      :: !Text        -- ^ Operation name (e.g., "get", "put")
  , enirOpParam     :: !Text        -- ^ Parameter name for operation argument
  , enirOpParamType :: !ENIRType    -- ^ Type of the operation argument
  , enirOpResume    :: !Text        -- ^ Name of the resumption continuation
  , enirOpBody      :: !ENIRTerm    -- ^ Handler body (can call resume)
  } deriving stock (Eq, Show)

-- | Handler for the return value
-- Transforms the final result of the handled computation
data ENIRReturnHandler = ENIRReturnHandler
  { enirReturnParam     :: !Text      -- ^ Parameter name for return value
  , enirReturnParamType :: !ENIRType  -- ^ Type of the return value
  , enirReturnBody      :: !ENIRTerm  -- ^ Transformation of the return value
  } deriving stock (Eq, Show)

-- | Transform Core terms to ENIR (placeholder implementation)
toENIR :: Core.Term -> ENIRTerm
toENIR = transformTerm

transformTerm :: Core.Term -> ENIRTerm
transformTerm term = case term of
  TmVar name idx ->
    ENIRReturn (ENIRVVar name idx)

  TmLam name ty body ->
    ENIRReturn (ENIRVLam name (transformType ty) (transformTerm body))

  TmApp func arg ->
    -- ANF transformation would be more complex
    let func' = transformTerm func
        arg' = transformToValue arg
    in ENIRApp (extractLam func') arg'

  TmLet name ty val body ->
    ENIRLet name (transformType ty) (transformTerm val) (transformTerm body)

  TmTyAbs _ _ body ->
    -- Type abstraction is erased
    transformTerm body

  TmTyApp tm _ ->
    -- Type application is erased
    transformTerm tm

  TmCon name _ args ->
    ENIRReturn (ENIRVCon name (map transformToValue args))

  TmMatch subj _ cases ->
    ENIRMatch (transformToValue subj) (map transformCase cases)

  TmPerform effect op arg ->
    -- Transform to CPS-style effect call
    ENIRCall effect op (transformToValue arg) (Continuation "result" (ENIRReturn (ENIRVVar "result" 0)))

  TmHandle handler body ->
    -- Transform handler and wrap the body
    ENIRHandle (transformHandler handler) (transformTerm body)

  TmLazy body ->
    -- Lazy becomes a lambda
    ENIRReturn (ENIRVLam "_" (ENIRTyCon "Unit" []) (transformTerm body))

  TmForce tm ->
    -- Force becomes application to unit
    ENIRApp (extractLam (transformTerm tm)) (ENIRVCon "Unit" [])

  TmAnnot tm _ ->
    transformTerm tm

  TmExternal binding args ->
    transformExternalCall binding args

-- | Transform an external function call to ENIR
-- External calls become a sequence of argument evaluations followed by a call
transformExternalCall :: ExternalBinding -> [Core.Term] -> ENIRTerm
transformExternalCall binding args =
  let argVals = map transformToValue args
      -- External calls are represented as effect-like calls to a special "FFI" effect
      -- The module and function name are combined into the operation name
      effectName = "FFI"
      opName = extBindModule binding <> "." <> extBindFunction binding
      -- Pack arguments into a tuple-like constructor
      argVal = case argVals of
        [] -> ENIRVCon "Unit" []
        [v] -> v
        vs -> ENIRVCon "_Args" vs
  in ENIRCall effectName opName argVal
       (Continuation "result" (ENIRReturn (ENIRVVar "result" 0)))

transformType :: Core.Type -> ENIRType
transformType ty = case ty of
  TyCon name args -> ENIRTyCon name (map transformType args)
  TyPi _ from _ to -> ENIRTyFn (transformType from) (transformType to)
  TyForall _ _ body -> transformType body
  TyForallDep _ _ body -> transformType body
  TyLazy inner -> ENIRTyLazy (transformType inner)
  TyLinear inner -> transformType inner
  TyRef inner -> transformType inner
  TyRefMut inner -> transformType inner
  TyUniverse _ -> ENIRTyCon "Type" []
  TyProp -> ENIRTyCon "Unit" []  -- Prop is erased to Unit
  TyVar name _ -> ENIRTyCon name []  -- Type variables become concrete
  TySigma _ fst snd -> ENIRTyCon "Pair" [transformType fst, transformType snd]
  TyNatLit n -> ENIRTyCon ("Nat_" <> T.pack (show n)) []
  TyAdd left right -> ENIRTyCon "Nat" []  -- Type-level addition erased
  TyEffect _ inner -> transformType inner  -- Effect annotation erased
  TyRefined inner _ -> transformType inner  -- Refinement predicates erased

-- | Transform a Core term to an ENIR value
-- For simple terms (variables, constructors, lambdas), returns the value directly
-- For complex terms that require computation, we need to lift them into let bindings
-- at the call site. This function provides a fallback for direct value extraction.
transformToValue :: Core.Term -> ENIRValue
transformToValue term = case term of
  -- Direct values
  TmVar name idx -> ENIRVVar name idx
  TmCon name _ args -> ENIRVCon name (map transformToValue args)
  TmLam name ty body -> ENIRVLam name (transformType ty) (transformTerm body)

  -- Type erasure - look through
  TmTyAbs _ _ body -> transformToValue body
  TmTyApp tm _ -> transformToValue tm
  TmAnnot tm _ -> transformToValue tm

  -- Lazy becomes a thunk value
  TmLazy body -> ENIRVLam "_" (ENIRTyCon "Unit" []) (transformTerm body)

  -- For complex terms that require computation, we create a thunk
  -- This preserves semantics by delaying the computation
  TmApp func arg ->
    -- Wrap application in a thunk: \_ -> func arg
    ENIRVLam "_thunk" (ENIRTyCon "Unit" [])
      (ENIRApp (extractLam (transformTerm func)) (transformToValue arg))

  TmLet name ty val body ->
    -- Wrap let in a thunk: \_ -> let x = v in body
    ENIRVLam "_thunk" (ENIRTyCon "Unit" [])
      (ENIRLet name (transformType ty) (transformTerm val) (transformTerm body))

  TmMatch subj ty cases ->
    -- Wrap match in a thunk: \_ -> match subj { cases }
    ENIRVLam "_thunk" (ENIRTyCon "Unit" [])
      (ENIRMatch (transformToValue subj) (map transformCase cases))

  TmPerform effect op arg ->
    -- Wrap effect call in a thunk: \_ -> perform effect.op(arg)
    ENIRVLam "_thunk" (ENIRTyCon "Unit" [])
      (ENIRCall effect op (transformToValue arg)
        (Continuation "result" (ENIRReturn (ENIRVVar "result" 0))))

  TmHandle handler body ->
    -- Wrap handler in a thunk: \_ -> handle handler in body
    ENIRVLam "_thunk" (ENIRTyCon "Unit" [])
      (ENIRHandle (transformHandler handler) (transformTerm body))

  TmForce tm ->
    -- Wrap force in a thunk: \_ -> force tm
    ENIRVLam "_thunk" (ENIRTyCon "Unit" [])
      (ENIRApp (extractLam (transformTerm tm)) (ENIRVCon "Unit" []))

  TmExternal binding args ->
    -- Wrap external call in a thunk: \_ -> extern binding(args)
    ENIRVLam "_thunk" (ENIRTyCon "Unit" [])
      (transformExternalCall binding args)

transformCase :: Core.Case -> (Pattern, ENIRTerm)
transformCase (Case pat body) = (pat, transformTerm body)

extractLam :: ENIRTerm -> ENIRTerm
extractLam (ENIRReturn (ENIRVLam name ty body)) = ENIRLam name ty body
extractLam other = other

-- | Transform a Core handler to ENIR representation
-- Converts operation handlers to CPS-style with explicit resumption continuations
transformHandler :: Handler -> ENIRHandler
transformHandler handler = ENIRHandler
  { enirHandlerEffect = handlerEffect handler
  , enirHandlerOps    = map transformOpHandler (handlerOperations handler)
  , enirHandlerReturn = transformReturnHandler (handlerReturn handler)
  }

-- | Transform an operation handler
-- The resumption becomes an explicit continuation parameter
transformOpHandler :: OpHandler -> ENIROpHandler
transformOpHandler opHandler = ENIROpHandler
  { enirOpName      = opHandlerOperation opHandler
  , enirOpParam     = patternToName (opHandlerPattern opHandler)
  , enirOpParamType = patternToType (opHandlerPattern opHandler)
  , enirOpResume    = opHandlerResume opHandler
  , enirOpBody      = transformTerm (opHandlerBody opHandler)
  }

-- | Transform a return handler
transformReturnHandler :: ReturnHandler -> ENIRReturnHandler
transformReturnHandler retHandler = ENIRReturnHandler
  { enirReturnParam     = patternToName (returnHandlerPattern retHandler)
  , enirReturnParamType = patternToType (returnHandlerPattern retHandler)
  , enirReturnBody      = transformTerm (returnHandlerBody retHandler)
  }

-- | Extract a name from a pattern (for simple variable patterns)
patternToName :: Pattern -> Text
patternToName (PatVar name) = name
patternToName PatWild = "_"
patternToName (PatCon _ _) = "_arg"  -- Constructor patterns get generic name

-- | Extract a type from a pattern (placeholder - patterns don't carry types)
patternToType :: Pattern -> ENIRType
patternToType _ = ENIRTyCon "Any" []  -- Placeholder type, would be inferred

--------------------------------------------------------------------------------
-- ANF Transformation
--------------------------------------------------------------------------------

-- | Result of normalizing a term to a value
-- Either the term is already a value, or we need let bindings
data NormalizeResult
  = NormValue !ENIRValue
    -- ^ Term is already a value
  | NormComplex !Text !ENIRTerm
    -- ^ Term requires computation; provides a fresh name and the computation
  deriving stock (Eq, Show)

-- | State for generating fresh variable names
type FreshM = State Int

-- | Generate a fresh variable name
freshVar :: Text -> FreshM Text
freshVar prefix = do
  n <- get
  put (n + 1)
  pure $ prefix <> "_" <> T.pack (show n)

-- | Run a FreshM computation
runFresh :: FreshM a -> a
runFresh m = fst (runState m 0)

-- | Normalize a Core term to an ENIR value, potentially introducing let bindings
-- This implements A-Normal Form (ANF) transformation for complex subexpressions
normalizeToValue :: Core.Term -> NormalizeResult
normalizeToValue term = case term of
  -- Simple values - no computation needed
  TmVar name idx ->
    NormValue (ENIRVVar name idx)

  TmCon name _ args ->
    -- Constructor with simple args becomes a value
    -- Complex args would need ANF transformation
    case normalizeArgs args of
      Left vals -> NormValue (ENIRVCon name vals)
      Right (bindings, vals) ->
        NormComplex "_con" (wrapLetBindings bindings (ENIRReturn (ENIRVCon name vals)))

  TmLam name ty body ->
    NormValue (ENIRVLam name (transformType ty) (transformTerm body))

  -- Type erasure - look through to the underlying term
  TmTyAbs _ _ body ->
    normalizeToValue body

  TmTyApp tm _ ->
    normalizeToValue tm

  TmAnnot tm _ ->
    normalizeToValue tm

  -- Complex terms require computation
  TmApp func arg ->
    NormComplex "_app" (transformTerm (TmApp func arg))

  TmLet name ty val body ->
    NormComplex "_let" (transformTerm (TmLet name ty val body))

  TmMatch subj ty cases ->
    NormComplex "_match" (transformTerm (TmMatch subj ty cases))

  TmPerform effect op arg ->
    NormComplex "_effect" (transformTerm (TmPerform effect op arg))

  TmHandle handler body ->
    NormComplex "_handle" (transformTerm (TmHandle handler body))

  TmLazy body ->
    -- Lazy is a value (thunk)
    NormValue (ENIRVLam "_" (ENIRTyCon "Unit" []) (transformTerm body))

  TmForce tm ->
    NormComplex "_force" (transformTerm (TmForce tm))

  TmExternal binding args ->
    NormComplex "_extern" (transformTerm (TmExternal binding args))

-- | Normalize a list of arguments, collecting any needed bindings
normalizeArgs :: [Core.Term] -> Either [ENIRValue] ([(Text, ENIRType, ENIRTerm)], [ENIRValue])
normalizeArgs args = runFresh (normalizeArgsM args)

-- | Monadic version of normalizeArgs
normalizeArgsM :: [Core.Term] -> FreshM (Either [ENIRValue] ([(Text, ENIRType, ENIRTerm)], [ENIRValue]))
normalizeArgsM args = do
  results <- mapM normalizeArgM args
  let (bindings, vals) = unzip results
  let allBindings = concat bindings
  if null allBindings
    then pure (Left vals)
    else pure (Right (allBindings, vals))

-- | Normalize a single argument
normalizeArgM :: Core.Term -> FreshM ([(Text, ENIRType, ENIRTerm)], ENIRValue)
normalizeArgM term = case normalizeToValue term of
  NormValue val ->
    pure ([], val)
  NormComplex prefix computation -> do
    name <- freshVar prefix
    let ty = ENIRTyCon "Any" []  -- Type would be inferred in a real implementation
    pure ([(name, ty, computation)], ENIRVVar name 0)

-- | Wrap a term in let bindings
wrapLetBindings :: [(Text, ENIRType, ENIRTerm)] -> ENIRTerm -> ENIRTerm
wrapLetBindings [] term = term
wrapLetBindings ((name, ty, val):rest) term =
  ENIRLet name ty val (wrapLetBindings rest term)
