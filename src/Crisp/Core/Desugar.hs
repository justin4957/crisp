{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Core.Desugar
-- Description : Surface to Core desugaring
--
-- Transforms the surface AST into the core calculus, removing
-- syntactic sugar like pipelines and do-notation.

module Crisp.Core.Desugar
  ( desugarModule
  , desugarExpr
  , desugarType
  , DesugarError(..)
  ) where

import qualified Crisp.Syntax.Surface as S
import qualified Crisp.Core.Term as C

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Desugaring errors
data DesugarError
  = UnboundVariable !Text
  | UnboundTypeVariable !Text
  | InvalidPattern !Text
  | Other !Text
  deriving stock (Eq, Show)

-- | Desugaring environment
data DesugarEnv = DesugarEnv
  { envTermVars :: !(Map Text Int)  -- ^ Term variable indices
  , envTypeVars :: !(Map Text Int)  -- ^ Type variable indices
  , envDepth    :: !Int             -- ^ Current binding depth
  } deriving stock (Eq, Show)

-- | Empty desugaring environment
emptyEnv :: DesugarEnv
emptyEnv = DesugarEnv Map.empty Map.empty 0

-- | Desugaring monad
type Desugar a = ReaderT DesugarEnv (Except DesugarError) a

-- | Run desugaring
runDesugar :: Desugar a -> Either DesugarError a
runDesugar = runExcept . flip runReaderT emptyEnv

-- | Extend environment with a term variable
extendTerm :: Text -> Desugar a -> Desugar a
extendTerm name = local $ \env ->
  let depth = envDepth env
  in env
    { envTermVars = Map.insert name depth (envTermVars env)
    , envDepth = depth + 1
    }

-- | Extend environment with a type variable
extendType :: Text -> Desugar a -> Desugar a
extendType name = local $ \env ->
  let depth = envDepth env
  in env
    { envTypeVars = Map.insert name depth (envTypeVars env)
    , envDepth = depth + 1
    }

-- | Look up a term variable
lookupTerm :: Text -> Desugar Int
lookupTerm name = do
  vars <- asks envTermVars
  depth <- asks envDepth
  case Map.lookup name vars of
    Just level -> pure (depth - level - 1)
    Nothing    -> throwError $ UnboundVariable name

-- | Look up a type variable
lookupType :: Text -> Desugar Int
lookupType name = do
  vars <- asks envTypeVars
  depth <- asks envDepth
  case Map.lookup name vars of
    Just level -> pure (depth - level - 1)
    Nothing    -> throwError $ UnboundTypeVariable name

-- | Desugar a module
desugarModule :: S.Module -> Either DesugarError [C.Term]
desugarModule m = runDesugar $ mapM desugarDef (S.moduleDefinitions m)

-- | Desugar a definition
desugarDef :: S.Definition -> Desugar C.Term
desugarDef = \case
  S.DefFn fn -> desugarFn fn
  S.DefType ty -> desugarTypeDef ty
  S.DefEffect _eff -> throwError $ Other "Effect definitions not yet implemented"
  S.DefHandler _h -> throwError $ Other "Handler definitions not yet implemented"
  S.DefTrait _t -> throwError $ Other "Trait definitions not yet implemented"
  S.DefImpl _i -> throwError $ Other "Impl definitions not yet implemented"
  S.DefExternal _e -> throwError $ Other "External definitions not yet implemented"
  S.DefTypeAlias a -> desugarTypeAlias a
  S.DefLet ld -> desugarLetDef ld

-- | Desugar a top-level let binding to a core term
desugarLetDef :: S.LetDef -> Desugar C.Term
desugarLetDef ld = do
  value <- desugarExpr (S.letDefValue ld)
  ty <- case S.letDefType ld of
    Just t  -> desugarType t
    Nothing -> pure $ C.TyVar "_infer" 0
  case S.letDefPattern ld of
    S.PatVar name _ -> pure $ C.TmLet name ty value (C.TmVar name 0)
    _ -> throwError $ InvalidPattern "Only variable patterns supported in top-level let"

-- | Desugar a type definition
-- Type definitions are desugared to a sequence of constructor functions.
-- Each constructor becomes a lambda that constructs a value of the type.
--
-- For example:
--   type Maybe A:
--     Nothing
--     Just(value: A)
--
-- Becomes (conceptually):
--   Nothing : Maybe A
--   Just : A -> Maybe A
desugarTypeDef :: S.TypeDef -> Desugar C.Term
desugarTypeDef td = do
  let typeName = S.typeDefName td
      params = S.typeDefParams td
      constructors = S.typeDefConstructors td

  -- Build the result type (the type being defined, applied to its parameters)
  resultType <- buildResultType typeName params

  -- Generate constructor terms
  -- For now, we emit a placeholder term that binds all constructors
  -- Each constructor is represented as a TmCon wrapped in appropriate lambdas
  conTerms <- mapM (desugarConstructor typeName resultType) constructors

  -- Combine all constructor definitions into a single term
  -- We use nested let bindings to introduce each constructor
  let combined = foldr combineConstructor (C.TmCon typeName [] []) conTerms
  pure combined
  where
    -- Build the result type: TypeName param1 param2 ...
    buildResultType name params = do
      let paramTypes = map paramToType params
      pure $ C.TyCon name paramTypes

    -- Convert a type parameter to a core type
    paramToType = \case
      S.TypeVar n _ _ -> C.TyVar n 0
      S.DepParam n _ _ -> C.TyVar n 0
      S.BoundedTypeVar n _ _ _ -> C.TyVar n 0

    -- Combine constructor terms using let bindings
    combineConstructor (conName, conTerm, conType) rest =
      C.TmLet conName conType conTerm rest

-- | Desugar a single constructor
-- Returns (constructor name, constructor term, constructor type)
desugarConstructor :: Text -> C.Type -> S.Constructor -> Desugar (Text, C.Term, C.Type)
desugarConstructor typeName resultType = \case
  S.SimpleConstructor name argTypes _ -> do
    -- For a constructor like Just(A), create a lambda: \(x: A) -> Just x
    argTypes' <- mapM desugarType argTypes
    let conTerm = buildConstructorLambda name argTypes'
        conType = buildConstructorType argTypes' resultType
    pure (name, conTerm, conType)

  S.GadtConstructor name ty _ -> do
    -- GADT constructor - the type is explicitly given
    ty' <- desugarType ty
    let conTerm = C.TmCon name [] []
    pure (name, conTerm, ty')

  S.RecordConstructor name fields _ -> do
    -- Record constructor: each field becomes a parameter
    let fieldNames = map S.fieldName fields
        fieldTypes = map S.fieldType fields
    fieldTypes' <- mapM desugarType fieldTypes
    let conTerm = buildRecordConstructorLambda name fieldNames fieldTypes'
        conType = buildConstructorType fieldTypes' resultType
    pure (name, conTerm, conType)
  where
    -- Build nested lambdas for constructor arguments
    buildConstructorLambda :: Text -> [C.Type] -> C.Term
    buildConstructorLambda name [] = C.TmCon name [] []
    buildConstructorLambda name argTypes =
      let argNames = zipWith (\_ i -> "_arg" <> T.pack (show i)) argTypes [0::Int ..]
          args = zipWith (\n _ -> C.TmVar n 0) argNames [0::Int ..]
          innerCon = C.TmCon name [] args
      in foldr (\(argName, argTy) body -> C.TmLam argName argTy body)
               innerCon
               (zip argNames argTypes)

    -- Build nested lambdas for record constructor
    buildRecordConstructorLambda :: Text -> [Text] -> [C.Type] -> C.Term
    buildRecordConstructorLambda name fieldNames fieldTypes =
      let args = zipWith (\n _ -> C.TmVar n 0) fieldNames [0::Int ..]
          innerCon = C.TmCon name [] args
      in foldr (\(fieldName, fieldTy) body -> C.TmLam fieldName fieldTy body)
               innerCon
               (zip fieldNames fieldTypes)

    -- Build the type of a constructor: arg1 -> arg2 -> ... -> ResultType
    buildConstructorType :: [C.Type] -> C.Type -> C.Type
    buildConstructorType [] result = result
    buildConstructorType (argTy:argTys) result =
      C.simpleFnType argTy (buildConstructorType argTys result) C.EffEmpty

-- | Desugar a type alias definition
-- Type aliases are mostly transparent - they just give a name to a type
desugarTypeAlias :: S.TypeAliasDef -> Desugar C.Term
desugarTypeAlias ta = do
  let aliasName = S.typeAliasName ta
  baseType <- desugarType (S.typeAliasBase ta)
  -- For now, emit a simple annotation that represents the alias
  -- The alias name is bound to its base type
  pure $ C.TmAnnot (C.TmCon aliasName [] []) baseType

-- | Desugar a function definition
-- Parameters must be bound in the environment before desugaring the body
desugarFn :: S.FunctionDef -> Desugar C.Term
desugarFn fn = do
  let params = S.fnDefParams fn
  -- Build nested lambdas with parameters in scope for body desugaring
  buildLambdas params
  where
    buildLambdas [] = desugarExpr (S.fnDefBody fn)
    buildLambdas (param:rest) = do
      ty <- desugarType (S.paramType param)
      let name = S.paramName param
      -- Extend environment with this parameter, then build rest
      extendTerm name $ do
        body <- buildLambdas rest
        pure $ C.TmLam name ty body

-- | Desugar an expression
desugarExpr :: S.Expr -> Desugar C.Term
desugarExpr = \case
  S.EVar name _ -> do
    idx <- lookupTerm name
    pure $ C.TmVar name idx

  S.ECon name _ ->
    pure $ C.TmCon name [] []

  S.EIntLit n _ ->
    -- Integers are represented as constructor applications
    pure $ C.TmCon "Int" [] []  -- Placeholder

  S.EFloatLit _ _ ->
    pure $ C.TmCon "Float" [] []  -- Placeholder

  S.EStringLit _ _ _ ->
    pure $ C.TmCon "String" [] []  -- Placeholder

  S.ECharLit _ _ ->
    pure $ C.TmCon "Char" [] []  -- Placeholder

  S.EUnit _ ->
    pure $ C.TmCon "Unit" [] []

  S.EApp func args _ -> do
    func' <- desugarExpr func
    args' <- mapM desugarExpr args
    -- Build nested applications
    pure $ foldl C.TmApp func' args'

  S.ELam _ params body _ ->
    foldr wrapParam (desugarExpr body) params
    where
      wrapParam param bodyM = do
        ty <- desugarType (S.paramType param)
        let name = S.paramName param
        extendTerm name $ do
          body' <- bodyM
          pure $ C.TmLam name ty body'

  S.ELet pat mTy value body _ -> do
    value' <- desugarExpr value
    ty <- case mTy of
      Just t  -> desugarType t
      Nothing -> pure $ C.TyVar "_infer" 0  -- Placeholder for inference
    case pat of
      S.PatVar name _ -> extendTerm name $ do
        body' <- desugarExpr body
        pure $ C.TmLet name ty value' body'
      _ -> throwError $ InvalidPattern "Only variable patterns supported in let"

  S.EAssign name value body _ -> do
    value' <- desugarExpr value
    let ty = C.TyVar "_infer" 0
    extendTerm name $ do
      body' <- desugarExpr body
      pure $ C.TmLet name ty value' body'

  S.EMatch subject arms _ -> do
    subject' <- desugarExpr subject
    arms' <- mapM desugarArm arms
    -- Return type will be inferred
    pure $ C.TmMatch subject' (C.TyVar "_infer" 0) arms'

  S.EIf cond then_ else_ _ -> do
    cond' <- desugarExpr cond
    then' <- desugarExpr then_
    else' <- desugarExpr else_
    -- Desugar to match on Bool
    pure $ C.TmMatch cond' (C.TyVar "_infer" 0)
      [ C.Case (C.PatCon "True" []) then'
      , C.Case (C.PatCon "False" []) else'
      ]

  S.EDo stmts result _ -> do
    result' <- desugarExpr result
    foldrM desugarDoStmt result' stmts

  S.EWith handler body _ -> do
    handler' <- desugarExpr handler
    body' <- desugarExpr body
    -- For now, just return the body (handler application is complex)
    pure body'  -- Placeholder

  S.EPerform effect op args _ -> do
    args' <- mapM desugarExpr args
    let arg = case args' of
          []  -> C.TmCon "Unit" [] []
          [a] -> a
          _   -> C.TmCon "Tuple" [] args'  -- Multiple args as tuple
    pure $ C.TmPerform effect op arg

  S.ELazy inner _ -> do
    inner' <- desugarExpr inner
    pure $ C.TmLazy inner'

  S.EForce inner _ -> do
    inner' <- desugarExpr inner
    pure $ C.TmForce inner'

  S.ERef inner _mut _ -> do
    -- Borrow expressions need special handling
    desugarExpr inner

  S.EPipe left right _ -> do
    -- x |> f  desugars to  f x
    left' <- desugarExpr left
    right' <- desugarExpr right
    pure $ C.TmApp right' left'

  S.EAnnot expr ty _ -> do
    expr' <- desugarExpr expr
    ty' <- desugarType ty
    pure $ C.TmAnnot expr' ty'

  S.EBlock stmts result _ -> do
    result' <- desugarExpr result
    foldrM desugarStmt result' stmts

  S.EQualified modPath name _ -> do
    -- Qualified names become simple variables after resolution
    idx <- lookupTerm name
    pure $ C.TmVar name idx

  S.EExternal extRef args _ -> do
    -- External function calls
    args' <- mapM desugarExpr args
    let extBinding = C.ExternalBinding
          (S.externalModule extRef)
          (S.externalFunction extRef)
          (C.TyVar "_infer" 0)  -- Type will be inferred
    pure $ C.TmExternal extBinding args'

  S.EFieldAccess expr field _ -> do
    -- Field access desugars to a function call: field_accessor expr
    expr' <- desugarExpr expr
    let accessorName = field  -- The field name acts as a function
    pure $ C.TmApp (C.TmVar accessorName 0) expr'

  S.EBinOp op left right _ -> do
    -- Binary operators desugar to function applications
    left' <- desugarExpr left
    right' <- desugarExpr right
    let opName = binOpToName op
    -- op left right  ->  (op left) right
    pure $ C.TmApp (C.TmApp (C.TmVar opName 0) left') right'

  S.ERecord conName fields _ -> do
    -- Record construction desugars to constructor application
    fields' <- mapM (\(_, e) -> desugarExpr e) fields
    pure $ C.TmCon conName [] fields'

-- | Convert binary operator to function name
binOpToName :: S.BinOp -> T.Text
binOpToName = \case
  S.OpAdd -> "add"
  S.OpSub -> "sub"
  S.OpMul -> "mul"
  S.OpDiv -> "div"
  S.OpMod -> "mod"
  S.OpAnd -> "and"
  S.OpOr  -> "or"
  S.OpLT  -> "lt"
  S.OpLE  -> "le"
  S.OpGT  -> "gt"
  S.OpGE  -> "ge"
  S.OpEQ  -> "eq"
  S.OpNE  -> "ne"

-- | Desugar a statement
desugarStmt :: S.Statement -> C.Term -> Desugar C.Term
desugarStmt stmt rest = case stmt of
  S.StmtLet pat mTy value _ -> do
    value' <- desugarExpr value
    ty <- maybe (pure $ C.TyVar "_infer" 0) desugarType mTy
    case pat of
      S.PatVar name _ -> extendTerm name $ do
        pure $ C.TmLet name ty value' rest
      _ -> throwError $ InvalidPattern "Only variable patterns in statements"
  S.StmtExpr expr _ -> do
    expr' <- desugarExpr expr
    -- Sequence: evaluate expr for effects, then continue with rest
    pure $ C.TmLet "_" (C.simpleType "Unit") expr' rest

-- | Desugar a do-statement
desugarDoStmt :: S.DoStatement -> C.Term -> Desugar C.Term
desugarDoStmt stmt rest = case stmt of
  S.DoBind pat effect op args _ -> do
    args' <- mapM desugarExpr args
    let arg = case args' of
          []  -> C.TmCon "Unit" [] []
          [a] -> a
          _   -> C.TmCon "Tuple" [] args'
    let perform = C.TmPerform effect op arg
    case pat of
      S.PatVar name _ -> extendTerm name $ do
        pure $ C.TmLet name (C.TyVar "_infer" 0) perform rest
      _ -> throwError $ InvalidPattern "Only variable patterns in do-bind"

  S.DoLet pat mTy value _ ->
    desugarStmt (S.StmtLet pat mTy value (S.fnDefSpan undefined)) rest

  S.DoExpr expr _ ->
    desugarStmt (S.StmtExpr expr (S.fnDefSpan undefined)) rest

-- | Desugar a match arm
desugarArm :: S.MatchArm -> Desugar C.Case
desugarArm arm = do
  pat <- desugarPattern (S.matchArmPattern arm)
  body <- extendPatternVars (S.matchArmPattern arm) $ desugarExpr (S.matchArmBody arm)
  pure $ C.Case pat body

-- | Desugar a pattern
desugarPattern :: S.Pattern -> Desugar C.Pattern
desugarPattern = \case
  S.PatVar name _ -> pure $ C.PatVar name
  S.PatWildcard _ -> pure C.PatWild
  S.PatCon name pats _ -> do
    pats' <- mapM desugarPattern pats
    pure $ C.PatCon name pats'
  S.PatTuple pats _ -> do
    pats' <- mapM desugarPattern pats
    pure $ C.PatCon "Tuple" pats'
  S.PatLit _ _ -> throwError $ InvalidPattern "Literal patterns not yet supported"
  S.PatTyped pat _ _ -> desugarPattern pat

-- | Extend environment with pattern-bound variables
extendPatternVars :: S.Pattern -> Desugar a -> Desugar a
extendPatternVars pat m = case pat of
  S.PatVar name _ -> extendTerm name m
  S.PatWildcard _ -> m
  S.PatCon _ pats _ -> foldr extendPatternVars m pats
  S.PatTuple pats _ -> foldr extendPatternVars m pats
  S.PatLit _ _ -> m
  S.PatTyped p _ _ -> extendPatternVars p m

-- | Desugar a type
desugarType :: S.Type -> Desugar C.Type
desugarType = \case
  S.TyName name _ ->
    -- Could be a type variable or type constructor
    -- For now, assume constructor
    pure $ C.simpleType name

  S.TyApp con args _ -> do
    con' <- desugarType con
    args' <- mapM desugarType args
    case con' of
      C.TyCon name _ -> pure $ C.TyCon name args'
      _ -> throwError $ Other "Invalid type application"

  S.TyFn from to effs _ -> do
    from' <- desugarType from
    to' <- desugarType to
    effs' <- desugarEffects effs
    pure $ C.simpleFnType from' to' effs'

  S.TyDepFn name paramTy retTy effs _ -> do
    paramTy' <- desugarType paramTy
    retTy' <- extendType name $ desugarType retTy
    effs' <- desugarEffects effs
    pure $ C.TyPi name paramTy' effs' retTy'

  S.TyForall param body _ -> do
    let (name, kind) = typeParamNameKind param
    kind' <- maybe (pure $ C.KiType 0) desugarKind kind
    body' <- extendType name $ desugarType body
    pure $ C.TyForall name kind' body'

  S.TyLazy inner _ -> do
    inner' <- desugarType inner
    pure $ C.TyLazy inner'

  S.TyRef inner mut _ -> do
    inner' <- desugarType inner
    pure $ if mut then C.TyRefMut inner' else C.TyRef inner'

  S.TyParen inner _ -> desugarType inner

  S.TyRefinement base _preds _ -> desugarType base  -- Refinement predicates not yet desugared

  S.TyHole _ -> pure $ C.TyVar "_infer" 0  -- Type hole: to be filled by inference

  S.TyTuple elems _ -> do
    elems' <- mapM desugarType elems
    pure $ C.TyCon "Tuple" elems'

  S.TyWild _ -> pure $ C.TyVar "_wild" 0  -- Wildcard type: to be filled by inference

-- | Extract name and kind from type parameter
typeParamNameKind :: S.TypeParam -> (Text, Maybe S.Kind)
typeParamNameKind = \case
  S.TypeVar name kind _ -> (name, kind)
  S.DepParam name _ _   -> (name, Nothing)

-- | Desugar effect references
desugarEffects :: [S.EffectRef] -> Desugar C.EffectRow
desugarEffects [] = pure C.EffEmpty
desugarEffects refs = pure $ C.EffSet $ map desugarEffectRef refs

desugarEffectRef :: S.EffectRef -> C.Effect
desugarEffectRef ref = C.Effect
  { C.effectName = S.effectRefName ref
  , C.effectAuthority = S.effectRefAuthority ref
  }

-- | Desugar a kind
desugarKind :: S.Kind -> Desugar C.Kind
desugarKind = \case
  S.KindType mLevel _ -> pure $ C.KiType (maybe 0 id mLevel)
  S.KindProp _ -> pure C.KiProp
  S.KindLinear _ -> pure C.KiLinear
  S.KindArrow k1 k2 _ -> do
    k1' <- desugarKind k1
    k2' <- desugarKind k2
    pure $ C.KiArrow k1' k2'

-- Helper for folding with monadic actions
foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ z []     = pure z
foldrM f z (x:xs) = do
  rest <- foldrM f z xs
  f x rest
