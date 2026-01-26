{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Crisp.Syntax.Surface
-- Description : Surface Abstract Syntax Tree
--
-- Represents the parsed structure of Crisp source code before
-- desugaring to the core calculus.

module Crisp.Syntax.Surface
  ( -- * Module
    Module(..)
  , ModulePath(..)
  , Require(..)
  , Provide(..)
    -- * Definitions
  , Definition(..)
  , TypeDef(..)
  , TypeModifiers(..)
  , TypeParam(..)
  , Constructor(..)
  , Field(..)
  , Kind(..)
  , EffectDef(..)
  , Operation(..)
  , HandlerDef(..)
  , HandlerParam(..)
  , HandlerClause(..)
  , FunctionDef(..)
  , Param(..)
  , EffectRef(..)
    -- * Types
  , Type(..)
    -- * Expressions
  , Expr(..)
  , Statement(..)
  , DoStatement(..)
  , MatchArm(..)
    -- * Patterns
  , Pattern(..)
  ) where

import Crisp.Syntax.Span (Span)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A module declaration
data Module = Module
  { moduleName       :: !ModulePath
  , moduleAuthority  :: !(Maybe Text)
  , moduleRequires   :: ![Require]
  , moduleProvides   :: ![Provide]
  , moduleDefinitions :: ![Definition]
  , moduleSpan       :: !Span
  } deriving stock (Eq, Show, Generic)

-- | A qualified module path (e.g., Treasury.Audit)
data ModulePath = ModulePath
  { modulePathSegments :: ![Text]
  , modulePathSpan     :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Module requirement declaration
data Require
  = RequireEffects ![Text] !Span
  | RequireTypes ![Text] !Span
  deriving stock (Eq, Show, Generic)

-- | Module provision declaration
data Provide
  = ProvideType !Text !Span
  | ProvideFn !Text !Type !Span
  deriving stock (Eq, Show, Generic)

-- | Top-level definition
data Definition
  = DefType !TypeDef
  | DefEffect !EffectDef
  | DefHandler !HandlerDef
  | DefFn !FunctionDef
  deriving stock (Eq, Show, Generic)

-- | Type definition (ADT, GADT, record, linear type)
data TypeDef = TypeDef
  { typeDefName         :: !Text
  , typeDefParams       :: ![TypeParam]
  , typeDefKind         :: !(Maybe Kind)
  , typeDefConstructors :: ![Constructor]
  , typeDefModifiers    :: !TypeModifiers
  , typeDefSpan         :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Modifiers on type definitions
data TypeModifiers = TypeModifiers
  { modifierIsProp   :: !Bool
  , modifierIsLinear :: !Bool
  } deriving stock (Eq, Show, Generic)

-- | A type parameter
data TypeParam
  = TypeVar !Text !(Maybe Kind) !Span       -- ^ Simple type variable: T
  | DepParam !Text !Type !Span              -- ^ Dependent parameter: (n: Nat)
  deriving stock (Eq, Show, Generic)

-- | A data constructor
data Constructor
  = SimpleConstructor !Text ![Type] !Span                     -- ^ Simple: Some T
  | GadtConstructor !Text !Type !Span                         -- ^ GADT: Cons : T -> Vec T m -> Vec T (succ m)
  | RecordConstructor !Text ![Field] !Span                    -- ^ Record style
  deriving stock (Eq, Show, Generic)

-- | A record field
data Field = Field
  { fieldName :: !Text
  , fieldType :: !Type
  , fieldSpan :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Kind expressions
data Kind
  = KindType !(Maybe Int) !Span             -- ^ Type_i
  | KindProp !Span                          -- ^ Prop
  | KindLinear !Span                        -- ^ Linear
  | KindArrow !Kind !Kind !Span             -- ^ k1 -> k2
  deriving stock (Eq, Show, Generic)

-- | Effect definition
data EffectDef = EffectDef
  { effectDefName       :: !Text
  , effectDefOperations :: ![Operation]
  , effectDefSpan       :: !Span
  } deriving stock (Eq, Show, Generic)

-- | An effect operation
data Operation = Operation
  { operationName      :: !Text
  , operationSignature :: !Type
  , operationSpan      :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Handler definition
data HandlerDef = HandlerDef
  { handlerDefName              :: !Text
  , handlerDefParams            :: ![HandlerParam]
  , handlerDefEffect            :: !Text
  , handlerDefIntroducedEffects :: ![EffectRef]
  , handlerDefClauses           :: ![HandlerClause]
  , handlerDefSpan              :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Handler parameter
data HandlerParam
  = HandlerTypeParam !Text !(Maybe Kind) !Span
  | HandlerValueParam !Text !Type !Span
  deriving stock (Eq, Show, Generic)

-- | A clause in a handler
data HandlerClause
  = OpClause !Text ![Pattern] !Text !Expr !Span  -- ^ op pattern -> resume: body
  | ReturnClause !Pattern !Expr !Span            -- ^ return pattern -> body
  deriving stock (Eq, Show, Generic)

-- | Function definition
data FunctionDef = FunctionDef
  { fnDefName       :: !Text
  , fnDefTypeParams :: ![TypeParam]
  , fnDefParams     :: ![Param]
  , fnDefReturnType :: !(Maybe Type)
  , fnDefEffects    :: ![EffectRef]
  , fnDefBody       :: !Expr
  , fnDefSpan       :: !Span
  } deriving stock (Eq, Show, Generic)

-- | A value parameter
data Param = Param
  { paramName :: !Text
  , paramType :: !Type
  , paramSpan :: !Span
  } deriving stock (Eq, Show, Generic)

-- | An effect reference (with optional authority)
data EffectRef = EffectRef
  { effectRefName      :: !Text
  , effectRefAuthority :: !(Maybe Text)
  , effectRefSpan      :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Type expressions
data Type
  = TyName !Text !Span                              -- ^ Type variable or constructor
  | TyApp !Type ![Type] !Span                       -- ^ Type application
  | TyFn !Type !Type ![EffectRef] !Span             -- ^ Function type: A -> B ! E
  | TyDepFn !Text !Type !Type ![EffectRef] !Span    -- ^ Dependent function: (x: A) -> B
  | TyForall !TypeParam !Type !Span                 -- ^ Forall quantification
  | TyLazy !Type !Span                              -- ^ Lazy T
  | TyRef !Type !Bool !Span                         -- ^ ref T or ref mut T
  | TyParen !Type !Span                             -- ^ Parenthesized type
  deriving stock (Eq, Show, Generic)

-- | Expressions
data Expr
  = EVar !Text !Span                                    -- ^ Variable reference
  | ECon !Text !Span                                    -- ^ Constructor reference
  | EIntLit !Integer !Span                              -- ^ Integer literal
  | EFloatLit !Double !Span                             -- ^ Float literal
  | EStringLit !Text !Span                              -- ^ String literal
  | ECharLit !Char !Span                                -- ^ Character literal
  | EUnit !Span                                         -- ^ Unit literal
  | EApp !Expr ![Expr] !Span                            -- ^ Function application
  | ELam ![Param] !Expr !Span                           -- ^ Lambda expression
  | ELet !Pattern !(Maybe Type) !Expr !Expr !Span       -- ^ Let binding
  | EMatch !Expr ![MatchArm] !Span                      -- ^ Match expression
  | EIf !Expr !Expr !Expr !Span                         -- ^ If-then-else
  | EDo ![DoStatement] !Expr !Span                      -- ^ Do block
  | EWith !Expr !Expr !Span                             -- ^ With handler
  | EPerform !Text !Text ![Expr] !Span                  -- ^ Perform effect operation
  | ELazy !Expr !Span                                   -- ^ Lazy expression
  | EForce !Expr !Span                                  -- ^ Force expression
  | ERef !Expr !Bool !Span                              -- ^ Reference (borrow)
  | EPipe !Expr !Expr !Span                             -- ^ Pipeline operator
  | EAnnot !Expr !Type !Span                            -- ^ Type annotation
  | EBlock ![Statement] !Expr !Span                     -- ^ Block expression
  | EQualified ![Text] !Text !Span                      -- ^ Qualified name
  deriving stock (Eq, Show, Generic)

-- | A statement in a block
data Statement
  = StmtLet !Pattern !(Maybe Type) !Expr !Span
  | StmtExpr !Expr !Span
  deriving stock (Eq, Show, Generic)

-- | A statement in a do-block
data DoStatement
  = DoBind !Pattern !Text !Text ![Expr] !Span    -- ^ pattern <- Effect.op args
  | DoLet !Pattern !(Maybe Type) !Expr !Span     -- ^ let pattern = expr
  | DoExpr !Expr !Span                           -- ^ expr
  deriving stock (Eq, Show, Generic)

-- | A match arm
data MatchArm = MatchArm
  { matchArmPattern :: !Pattern
  , matchArmGuard   :: !(Maybe Expr)
  , matchArmBody    :: !Expr
  , matchArmSpan    :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Patterns for matching
data Pattern
  = PatVar !Text !Span                          -- ^ Variable binding
  | PatWildcard !Span                           -- ^ Wildcard _
  | PatCon !Text ![Pattern] !Span               -- ^ Constructor pattern
  | PatTuple ![Pattern] !Span                   -- ^ Tuple pattern
  | PatLit !Expr !Span                          -- ^ Literal pattern
  | PatTyped !Pattern !Type !Span               -- ^ Type-annotated pattern
  deriving stock (Eq, Show, Generic)
