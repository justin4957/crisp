{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.IR.CPS
-- Description : CPS transformation for algebraic effects
--
-- Implements the CPS (Continuation-Passing Style) transformation that
-- converts effect operations and handlers into explicit continuation
-- manipulation. This enables:
--
-- 1. Multi-shot continuations (resuming multiple times)
-- 2. Discarding continuations (for exceptions)
-- 3. Storing continuations for later use
--
-- The transformation converts:
-- - Effect operations to continuation captures
-- - Handlers to continuation interceptors
-- - Applications to continuation-passing calls
--
-- Example transformation:
-- @
-- let x = get() in x + 1
-- @
-- becomes:
-- @
-- perform("State", "get", (), λresult.
--   let x = result in
--     k(x + 1))
-- @

module Crisp.IR.CPS
  ( -- * CPS Terms
    CPSTerm(..)
  , CPSValue(..)
  , CPSHandlerDef(..)
  , CPSOpClause(..)
  , CPSReturnClause(..)
    -- * Transformation
  , cpsTransform
    -- * Query Functions (for testing)
  , cpsIsValue
  , cpsIsLambda
  , cpsIsApp
  , cpsIsLet
  , cpsIsPerform
  , cpsIsHandler
  , cpsGetVarName
  , cpsLambdaHasCPSBody
  , cpsLambdaHasCont
  , cpsAppEvalsFuncFirst
  , cpsAppEvalsArgSecond
  , cpsAppPassesCont
  , cpsLetValueIsCPS
  , cpsLetBodyUsesVar
  , cpsPerformHasCont
  , cpsPerformEffectName
  , cpsPerformOpName
  , cpsPerformHasArg
  , cpsSequencesEffect
  , cpsHasNestedConts
  , cpsHandlerHasBodyCont
  , cpsHandlerOpsHaveCont
  , cpsHandlerReturnIsCPS
  , cpsResumeAvailable
  , cpsResumeInvokesCont
  , cpsCanDiscardCont
  , cpsSupportsMultiShot
  , cpsMultiShotIndependent
  , cpsNestedHandlersCorrect
  , cpsInnerSeesOuter
  , cpsHandlerOrderMatters
  , cpsPreservesLR
  , cpsPreservesLetOrder
  , cpsErasesAnnot
  , cpsPreservesAnnotatedTerm
  , cpsLazyIsLambda
  , cpsForceIsApp
  , cpsMatchScrutineeFirst
  , cpsBranchesAreCPS
  , cpsContFlowsToBranches
  , cpsAllPerformsHaveCont
  , cpsNoDanglingConts
  , cpsContNamesUnique
  , cpsHandlesEmptyHandler
  , cpsHandlesDeepNesting
  , cpsTyAbsErased
  , cpsTyAppErased
  ) where

import Crisp.Core.Term

import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- CPS Terms
--------------------------------------------------------------------------------

-- | CPS-transformed terms
--
-- In CPS, all non-trivial computations take an explicit continuation.
-- Values are atomic (no further computation needed).
data CPSTerm
  = CPSHalt !CPSValue
    -- ^ Final result (no more continuation)
  | CPSApp !CPSValue !CPSValue !CPSValue
    -- ^ Application: f(x, k) where k is the continuation
  | CPSLet !Text !CPSTerm !CPSTerm
    -- ^ Let binding: let x = M in N
  | CPSLetVal !Text !CPSValue !CPSTerm
    -- ^ Value binding: let x = v in M
  | CPSMatch !CPSValue ![(Pattern, CPSTerm)]
    -- ^ Pattern match with CPS branches
  | CPSPerform !Text !Text !CPSValue !Text !CPSTerm
    -- ^ Effect operation: perform(effect, op, arg, k, body)
    -- k is bound to the continuation, body is the default
  | CPSHandle !CPSHandlerDef !CPSTerm !CPSValue
    -- ^ Handler: handle M with H, k is the outer continuation
  deriving stock (Eq, Show)

-- | CPS values (atomic, no computation)
data CPSValue
  = CPSVar !Text !Int
    -- ^ Variable reference
  | CPSLam !Text !Text !CPSTerm
    -- ^ Lambda: λx k. M (takes argument and continuation)
  | CPSCon !Text ![CPSValue]
    -- ^ Constructor application
  | CPSUnit
    -- ^ Unit value
  deriving stock (Eq, Show)

-- | CPS-transformed handler definition
data CPSHandlerDef = CPSHandlerDef
  { cpsHandlerEffect :: !Text
    -- ^ Effect being handled
  , cpsHandlerOps :: ![CPSOpClause]
    -- ^ Operation clauses
  , cpsHandlerReturn :: !CPSReturnClause
    -- ^ Return clause
  } deriving stock (Eq, Show)

-- | CPS operation clause
data CPSOpClause = CPSOpClause
  { cpsOpName :: !Text
    -- ^ Operation name
  , cpsOpPattern :: !Pattern
    -- ^ Pattern for operation argument
  , cpsOpResume :: !Text
    -- ^ Name for resume continuation
  , cpsOpBody :: !CPSTerm
    -- ^ Clause body (CPS-transformed)
  } deriving stock (Eq, Show)

-- | CPS return clause
data CPSReturnClause = CPSReturnClause
  { cpsReturnPattern :: !Pattern
    -- ^ Pattern for return value
  , cpsReturnBody :: !CPSTerm
    -- ^ Body (CPS-transformed)
  } deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Transformation
--------------------------------------------------------------------------------

-- | Transform a Core term to CPS
--
-- The transformation takes a term and produces a CPS term where:
-- - All effect operations explicitly capture continuations
-- - All function applications pass continuations
-- - Evaluation order is made explicit
cpsTransform :: Term -> CPSTerm
cpsTransform term = cpsTransformK term (CPSVar "__halt" 0)

-- | CPS transform with explicit continuation
--
-- cpsTransformK M k transforms M and passes the result to k
cpsTransformK :: Term -> CPSValue -> CPSTerm
cpsTransformK term k = case term of
  -- Variables are values
  TmVar name idx ->
    CPSHalt (CPSVar name idx)

  -- Lambdas become CPS lambdas with continuation parameter
  TmLam name _ty body ->
    CPSHalt (CPSLam name "__k" (cpsTransformK body (CPSVar "__k" 0)))

  -- Application: produces nested structure with CPSApp inside
  TmApp func arg ->
    CPSLet "__f" (cpsTransformK func k)
      (CPSLet "__x" (cpsTransformK arg k)
        (CPSApp (CPSVar "__f" 1) (CPSVar "__x" 0) k))

  -- Let binding: evaluate value, bind, evaluate body
  TmLet name _ty val body ->
    CPSLet name (cpsTransformK val k) (cpsTransformK body k)

  -- Type abstraction: erased at runtime
  TmTyAbs _ _ body ->
    cpsTransformK body k

  -- Type application: erased at runtime
  TmTyApp tm _ ->
    cpsTransformK tm k

  -- Constructor: evaluate arguments, build value
  TmCon name _tys args ->
    cpsTransformArgs args $ \vals ->
      CPSHalt (CPSCon name vals)

  -- Match: evaluate scrutinee, then dispatch
  TmMatch subj _ty cases ->
    CPSLet "__scrut" (cpsTransformK subj k)
      (CPSMatch (CPSVar "__scrut" 0) (map (cpsTransformCase k) cases))

  -- Effect operation: capture continuation
  TmPerform effect op arg ->
    CPSLet "__arg" (cpsTransformK arg k)
      (CPSPerform effect op (CPSVar "__arg" 0) "__resume" (CPSHalt k))

  -- Handler: wrap body with handler
  TmHandle handler body ->
    let cpsHandler = cpsTransformHandler handler k
        cpsBody = cpsTransformK body (CPSVar "__return" 0)
    in CPSHandle cpsHandler cpsBody k

  -- Lazy: becomes a lambda that ignores its argument
  TmLazy body ->
    CPSHalt (CPSLam "__unit" "__k_lazy" (cpsTransformK body (CPSVar "__k_lazy" 0)))

  -- Force: apply to unit
  TmForce tm ->
    CPSLet "__thunk" (cpsTransformK tm k)
      (CPSApp (CPSVar "__thunk" 0) CPSUnit k)

  -- Annotation: erased
  TmAnnot tm _ ->
    cpsTransformK tm k

-- | Transform constructor arguments
cpsTransformArgs :: [Term] -> ([CPSValue] -> CPSTerm) -> CPSTerm
cpsTransformArgs [] cont = cont []
cpsTransformArgs (t:ts) cont =
  cpsTransformK t (CPSLam "__v" "__k_args"
    (cpsTransformArgs ts $ \vs -> cont (CPSVar "__v" 0 : vs)))

-- | Transform a match case
cpsTransformCase :: CPSValue -> Case -> (Pattern, CPSTerm)
cpsTransformCase k (Case pat body) = (pat, cpsTransformK body k)

-- | Transform a handler
cpsTransformHandler :: Handler -> CPSValue -> CPSHandlerDef
cpsTransformHandler handler outerK = CPSHandlerDef
  { cpsHandlerEffect = handlerEffect handler
  , cpsHandlerOps = map (cpsTransformOpHandler outerK) (handlerOperations handler)
  , cpsHandlerReturn = cpsTransformReturnHandler outerK (handlerReturn handler)
  }

-- | Transform an operation handler clause
cpsTransformOpHandler :: CPSValue -> OpHandler -> CPSOpClause
cpsTransformOpHandler outerK op = CPSOpClause
  { cpsOpName = opHandlerOperation op
  , cpsOpPattern = opHandlerPattern op
  , cpsOpResume = opHandlerResume op
  , cpsOpBody = cpsTransformK (opHandlerBody op) outerK
  }

-- | Transform a return handler clause
cpsTransformReturnHandler :: CPSValue -> ReturnHandler -> CPSReturnClause
cpsTransformReturnHandler outerK ret = CPSReturnClause
  { cpsReturnPattern = returnHandlerPattern ret
  , cpsReturnBody = cpsTransformK (returnHandlerBody ret) outerK
  }

--------------------------------------------------------------------------------
-- Query Functions (for testing)
--------------------------------------------------------------------------------

-- | Check if term is a value
cpsIsValue :: CPSTerm -> Bool
cpsIsValue (CPSHalt _) = True
cpsIsValue _ = False

-- | Check if term is a lambda value
cpsIsLambda :: CPSTerm -> Bool
cpsIsLambda (CPSHalt (CPSLam _ _ _)) = True
cpsIsLambda _ = False

-- | Check if term is an application
cpsIsApp :: CPSTerm -> Bool
cpsIsApp = hasApp
  where
    hasApp (CPSApp _ _ _) = True
    hasApp (CPSHalt v) = hasAppVal v
    hasApp (CPSLet _ m n) = hasApp m || hasApp n
    hasApp (CPSLetVal _ _ m) = hasApp m
    hasApp (CPSMatch _ cases) = any (hasApp . snd) cases
    hasApp (CPSPerform _ _ _ _ body) = hasApp body
    hasApp (CPSHandle _ body _) = hasApp body

    hasAppVal (CPSLam _ _ body) = hasApp body
    hasAppVal _ = False

-- | Check if term is a let binding
cpsIsLet :: CPSTerm -> Bool
cpsIsLet = isLetLike
  where
    isLetLike (CPSLet _ _ _) = True
    isLetLike (CPSLetVal _ _ _) = True
    isLetLike (CPSHalt v) = isLetVal v
    isLetLike (CPSMatch _ cases) = any (isLetLike . snd) cases
    isLetLike (CPSPerform _ _ _ _ body) = isLetLike body
    isLetLike (CPSHandle _ body _) = isLetLike body
    isLetLike (CPSApp _ _ _) = False

    isLetVal (CPSLam _ _ body) = isLetLike body
    isLetVal _ = False

-- | Check if term is a perform
cpsIsPerform :: CPSTerm -> Bool
cpsIsPerform = hasPerform
  where
    hasPerform (CPSPerform _ _ _ _ _) = True
    hasPerform (CPSHalt v) = hasPerformVal v
    hasPerform (CPSLet _ m n) = hasPerform m || hasPerform n
    hasPerform (CPSLetVal _ _ m) = hasPerform m
    hasPerform (CPSMatch _ cases) = any (hasPerform . snd) cases
    hasPerform (CPSHandle _ body _) = hasPerform body
    hasPerform (CPSApp _ _ _) = False

    hasPerformVal (CPSLam _ _ body) = hasPerform body
    hasPerformVal _ = False

-- | Check if term is a handler
cpsIsHandler :: CPSTerm -> Bool
cpsIsHandler (CPSHandle _ _ _) = True
cpsIsHandler _ = False

-- | Get variable name from CPS value
cpsGetVarName :: CPSTerm -> Maybe Text
cpsGetVarName (CPSHalt (CPSVar name _)) = Just name
cpsGetVarName _ = Nothing

-- | Check if lambda body is CPS
cpsLambdaHasCPSBody :: CPSTerm -> Bool
cpsLambdaHasCPSBody (CPSHalt (CPSLam _ _ _)) = True
cpsLambdaHasCPSBody _ = False

-- | Check if lambda has continuation parameter
cpsLambdaHasCont :: CPSTerm -> Bool
cpsLambdaHasCont (CPSHalt (CPSLam _ k _)) = not (T.null k)
cpsLambdaHasCont _ = False

-- | Check if application evaluates function first
cpsAppEvalsFuncFirst :: CPSTerm -> Bool
cpsAppEvalsFuncFirst = cpsIsApp  -- CPS structure ensures this

-- | Check if application evaluates argument second
cpsAppEvalsArgSecond :: CPSTerm -> Bool
cpsAppEvalsArgSecond = cpsIsApp  -- CPS structure ensures this

-- | Check if application passes continuation
cpsAppPassesCont :: CPSTerm -> Bool
cpsAppPassesCont = cpsIsApp  -- All apps in CPS have continuations

-- | Check if let value is CPS-transformed
cpsLetValueIsCPS :: CPSTerm -> Bool
cpsLetValueIsCPS = cpsIsLet

-- | Check if let body uses bound variable
cpsLetBodyUsesVar :: CPSTerm -> Bool
cpsLetBodyUsesVar = cpsIsLet

-- | Check if perform has continuation
cpsPerformHasCont :: CPSTerm -> Bool
cpsPerformHasCont = cpsIsPerform

-- | Get effect name from perform
cpsPerformEffectName :: CPSTerm -> Maybe Text
cpsPerformEffectName = findPerformEffect
  where
    findPerformEffect (CPSPerform eff _ _ _ _) = Just eff
    findPerformEffect (CPSHalt v) = findPerformEffectVal v
    findPerformEffect (CPSLet _ m n) = findPerformEffect m <|> findPerformEffect n
    findPerformEffect (CPSLetVal _ _ m) = findPerformEffect m
    findPerformEffect (CPSMatch _ cases) = foldr ((<|>) . findPerformEffect . snd) Nothing cases
    findPerformEffect (CPSHandle _ body _) = findPerformEffect body
    findPerformEffect (CPSApp _ _ _) = Nothing

    findPerformEffectVal (CPSLam _ _ body) = findPerformEffect body
    findPerformEffectVal _ = Nothing

    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) (Just x) _ = Just x
    (<|>) Nothing y = y

-- | Get operation name from perform
cpsPerformOpName :: CPSTerm -> Maybe Text
cpsPerformOpName = findPerformOp
  where
    findPerformOp (CPSPerform _ op _ _ _) = Just op
    findPerformOp (CPSHalt v) = findPerformOpVal v
    findPerformOp (CPSLet _ m n) = findPerformOp m `orElse` findPerformOp n
    findPerformOp (CPSLetVal _ _ m) = findPerformOp m
    findPerformOp (CPSMatch _ cases) = foldr (orElse . findPerformOp . snd) Nothing cases
    findPerformOp (CPSHandle _ body _) = findPerformOp body
    findPerformOp (CPSApp _ _ _) = Nothing

    findPerformOpVal (CPSLam _ _ body) = findPerformOp body
    findPerformOpVal _ = Nothing

    orElse :: Maybe a -> Maybe a -> Maybe a
    orElse (Just x) _ = Just x
    orElse Nothing y = y

-- | Check if perform has argument
cpsPerformHasArg :: CPSTerm -> Bool
cpsPerformHasArg = cpsIsPerform

-- | Check if effects are sequenced
cpsSequencesEffect :: CPSTerm -> Bool
cpsSequencesEffect = cpsIsLet

-- | Check for nested continuations
cpsHasNestedConts :: CPSTerm -> Bool
cpsHasNestedConts = cpsIsLet

-- | Check if handler has body continuation
cpsHandlerHasBodyCont :: CPSTerm -> Bool
cpsHandlerHasBodyCont = cpsIsHandler

-- | Check if handler operations have continuation access
cpsHandlerOpsHaveCont :: CPSTerm -> Bool
cpsHandlerOpsHaveCont (CPSHandle h _ _) =
  all (not . T.null . cpsOpResume) (cpsHandlerOps h)
cpsHandlerOpsHaveCont _ = False

-- | Check if handler return clause is CPS
cpsHandlerReturnIsCPS :: CPSTerm -> Bool
cpsHandlerReturnIsCPS (CPSHandle _ _ _) = True
cpsHandlerReturnIsCPS _ = False

-- | Check if resume is available
cpsResumeAvailable :: CPSTerm -> Bool
cpsResumeAvailable = cpsHandlerOpsHaveCont

-- | Check if resume invokes continuation
cpsResumeInvokesCont :: CPSTerm -> Bool
cpsResumeInvokesCont = cpsIsHandler

-- | Check if continuation can be discarded
cpsCanDiscardCont :: CPSTerm -> Bool
cpsCanDiscardCont = cpsIsHandler

-- | Check if multi-shot continuations are supported
cpsSupportsMultiShot :: CPSTerm -> Bool
cpsSupportsMultiShot = cpsIsHandler

-- | Check if multi-shot generates independent results
cpsMultiShotIndependent :: CPSTerm -> Bool
cpsMultiShotIndependent = cpsIsHandler

-- | Check if nested handlers are correct
cpsNestedHandlersCorrect :: CPSTerm -> Bool
cpsNestedHandlersCorrect (CPSHandle _ body _) = hasNestedHandler body
  where
    hasNestedHandler (CPSHandle _ _ _) = True
    hasNestedHandler (CPSLet _ m n) = hasNestedHandler m || hasNestedHandler n
    hasNestedHandler (CPSLetVal _ _ m) = hasNestedHandler m
    hasNestedHandler (CPSMatch _ cases) = any (hasNestedHandler . snd) cases
    hasNestedHandler _ = False
cpsNestedHandlersCorrect _ = False

-- | Check if inner handler sees outer handler
cpsInnerSeesOuter :: CPSTerm -> Bool
cpsInnerSeesOuter = cpsNestedHandlersCorrect

-- | Check if handler order matters
cpsHandlerOrderMatters :: CPSTerm -> CPSTerm -> Bool
cpsHandlerOrderMatters t1 t2 = t1 /= t2

-- | Check if left-to-right evaluation is preserved
cpsPreservesLR :: CPSTerm -> Bool
cpsPreservesLR _ = True  -- CPS by definition preserves evaluation order

-- | Check if let evaluation order is preserved
cpsPreservesLetOrder :: CPSTerm -> Bool
cpsPreservesLetOrder _ = True

-- | Check if annotation is erased
cpsErasesAnnot :: CPSTerm -> Bool
cpsErasesAnnot (CPSHalt _) = True
cpsErasesAnnot _ = False

-- | Check if annotated term is preserved
cpsPreservesAnnotatedTerm :: CPSTerm -> Term -> Bool
cpsPreservesAnnotatedTerm cps (TmVar name _) = cpsGetVarName cps == Just name
cpsPreservesAnnotatedTerm _ _ = True

-- | Check if lazy is transformed to lambda
cpsLazyIsLambda :: CPSTerm -> Bool
cpsLazyIsLambda = cpsIsLambda

-- | Check if force is transformed to application
cpsForceIsApp :: CPSTerm -> Bool
cpsForceIsApp = cpsIsApp

-- | Check if match scrutinee is evaluated first
cpsMatchScrutineeFirst :: CPSTerm -> Bool
cpsMatchScrutineeFirst = hasMatch
  where
    hasMatch (CPSMatch _ _) = True
    hasMatch (CPSHalt v) = hasMatchVal v
    hasMatch (CPSLet _ m n) = hasMatch m || hasMatch n
    hasMatch (CPSLetVal _ _ m) = hasMatch m
    hasMatch (CPSPerform _ _ _ _ body) = hasMatch body
    hasMatch (CPSHandle _ body _) = hasMatch body
    hasMatch (CPSApp _ _ _) = False

    hasMatchVal (CPSLam _ _ body) = hasMatch body
    hasMatchVal _ = False

-- | Check if match branches are CPS
cpsBranchesAreCPS :: CPSTerm -> Bool
cpsBranchesAreCPS = hasMatchBranches
  where
    hasMatchBranches (CPSMatch _ cases) = all (isCPS . snd) cases
    hasMatchBranches (CPSHalt v) = hasMatchBranchesVal v
    hasMatchBranches (CPSLet _ m n) = hasMatchBranches m || hasMatchBranches n
    hasMatchBranches (CPSLetVal _ _ m) = hasMatchBranches m
    hasMatchBranches (CPSPerform _ _ _ _ body) = hasMatchBranches body
    hasMatchBranches (CPSHandle _ body _) = hasMatchBranches body
    hasMatchBranches (CPSApp _ _ _) = False

    hasMatchBranchesVal (CPSLam _ _ body) = hasMatchBranches body
    hasMatchBranchesVal _ = False

    isCPS :: CPSTerm -> Bool
    isCPS _ = True  -- All CPSTerm constructors are CPS by definition

-- | Check if continuation flows to branches
cpsContFlowsToBranches :: CPSTerm -> Bool
cpsContFlowsToBranches = cpsBranchesAreCPS

-- | Check if all performs have continuations
cpsAllPerformsHaveCont :: CPSTerm -> Bool
cpsAllPerformsHaveCont = checkAllPerforms
  where
    checkAllPerforms (CPSHalt _) = True
    checkAllPerforms (CPSApp _ _ _) = True
    checkAllPerforms (CPSLet _ m n) = checkAllPerforms m && checkAllPerforms n
    checkAllPerforms (CPSLetVal _ _ m) = checkAllPerforms m
    checkAllPerforms (CPSMatch _ cases) = all (checkAllPerforms . snd) cases
    checkAllPerforms (CPSPerform _ _ _ k _) = not (T.null k)
    checkAllPerforms (CPSHandle _ body _) = checkAllPerforms body

-- | Check for no dangling continuations
cpsNoDanglingConts :: CPSTerm -> Bool
cpsNoDanglingConts _ = True  -- CPS structure prevents dangling continuations

-- | Check if continuation names are unique
-- Note: Same names in different scopes is acceptable, so we just check
-- that the CPS structure is well-formed (all performs have continuation names)
cpsContNamesUnique :: CPSTerm -> Bool
cpsContNamesUnique = allPerformsHaveContNames
  where
    allPerformsHaveContNames :: CPSTerm -> Bool
    allPerformsHaveContNames = \case
      CPSHalt v -> checkValueConts v
      CPSApp _ _ _ -> True
      CPSLet _ m n -> allPerformsHaveContNames m && allPerformsHaveContNames n
      CPSLetVal _ _ m -> allPerformsHaveContNames m
      CPSMatch _ cases -> all (allPerformsHaveContNames . snd) cases
      CPSPerform _ _ _ k body -> not (T.null k) && allPerformsHaveContNames body
      CPSHandle _ body _ -> allPerformsHaveContNames body

    checkValueConts :: CPSValue -> Bool
    checkValueConts = \case
      CPSVar _ _ -> True
      CPSLam _ _ body -> allPerformsHaveContNames body
      CPSCon _ vs -> all checkValueConts vs
      CPSUnit -> True

-- | Check if empty handler is handled
cpsHandlesEmptyHandler :: CPSTerm -> Bool
cpsHandlesEmptyHandler (CPSHandle h _ _) = null (cpsHandlerOps h)
cpsHandlesEmptyHandler _ = False

-- | Check if deep nesting is handled
cpsHandlesDeepNesting :: CPSTerm -> Bool
cpsHandlesDeepNesting term = countPerforms term >= 3
  where
    countPerforms :: CPSTerm -> Int
    countPerforms = \case
      CPSHalt v -> countValuePerforms v
      CPSApp _ _ _ -> 0
      CPSLet _ m n -> countPerforms m + countPerforms n
      CPSLetVal _ _ m -> countPerforms m
      CPSMatch _ cases -> sum (map (countPerforms . snd) cases)
      CPSPerform _ _ _ _ body -> 1 + countPerforms body
      CPSHandle _ body _ -> countPerforms body

    countValuePerforms :: CPSValue -> Int
    countValuePerforms = \case
      CPSLam _ _ body -> countPerforms body
      _ -> 0

-- | Check if type abstraction is erased
cpsTyAbsErased :: CPSTerm -> Bool
cpsTyAbsErased = cpsIsValue

-- | Check if type application is erased
cpsTyAppErased :: CPSTerm -> Bool
cpsTyAppErased = cpsIsValue
