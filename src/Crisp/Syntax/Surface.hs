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
    -- * Doc Comments
  , DocComment
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
    -- * Traits
  , TraitDef(..)
  , TraitMethod(..)
  , TraitMethodStyle(..)
  , ImplDef(..)
  , TraitConstraint(..)
  , DerivingClause(..)
    -- * Type Aliases
  , TypeAliasDef(..)
  , FieldConstraint(..)
    -- * FFI
  , ExternalRef(..)
  , ExternalFnDef(..)
    -- * Refinement Types
  , RefinementPredicate(..)
  , ComparisonOp(..)
    -- * Types
  , Type(..)
    -- * Expressions
  , Expr(..)
  , LambdaStyle(..)
  , BinOp(..)
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
  , moduleDocComment :: !(Maybe DocComment)
  } deriving stock (Eq, Show, Generic)

-- | A qualified module path (e.g., Treasury.Audit)
data ModulePath = ModulePath
  { modulePathSegments :: ![Text]
  , modulePathSpan     :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Module requirement declaration
data Require
  = RequireModule !ModulePath !Span       -- ^ Module import: requires LexSim.Core.Refined
  | RequireEffects ![Text] !Span          -- ^ Effect requirements: requires effects: E1, E2
  | RequireTypes ![Text] !Span            -- ^ Type requirements: requires types: T1, T2
  deriving stock (Eq, Show, Generic)

-- | Module provision declaration
data Provide
  = ProvideType !Text !Span                 -- ^ Export a type: provides type Name
  | ProvideTypeProp !Text !Span             -- ^ Export a prop type: provides type prop Name
  | ProvideFn !Text !(Maybe Type) !Span     -- ^ Export a function: provides fn name or provides fn name: Type
  | ProvideExternalFn !Text !(Maybe Type) !Span  -- ^ Export an external function: provides external fn name
  | ProvideEffect !Text !Span               -- ^ Export an effect: provides effect Name
  | ProvideTrait !Text !Span                -- ^ Export a trait: provides trait Name
  | ProvideHandler !Text !Span              -- ^ Export a handler: provides handler Name
  deriving stock (Eq, Show, Generic)

-- | A doc comment (--- | text)
type DocComment = Text

-- | Top-level definition
data Definition
  = DefType !TypeDef
  | DefEffect !EffectDef
  | DefHandler !HandlerDef
  | DefFn !FunctionDef
  | DefTrait !TraitDef
  | DefImpl !ImplDef
  | DefExternal !ExternalFnDef
  | DefTypeAlias !TypeAliasDef
  deriving stock (Eq, Show, Generic)

-- | Type definition (ADT, GADT, record, linear type)
data TypeDef = TypeDef
  { typeDefDocComment   :: !(Maybe DocComment)
  , typeDefName         :: !Text
  , typeDefParams       :: ![TypeParam]
  , typeDefConstraints  :: ![TraitConstraint]    -- ^ Where clause constraints
  , typeDefKind         :: !(Maybe Kind)
  , typeDefConstructors :: ![Constructor]
  , typeDefModifiers    :: !TypeModifiers
  , typeDefDeriving     :: !(Maybe DerivingClause)
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
  | BoundedTypeVar !Text !(Maybe Kind) ![Text] !Span
    -- ^ Bounded type variable with trait constraints: (A: Trait1 + Trait2)
    -- The list of Text contains trait names that the type must implement
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
  { effectDefDocComment  :: !(Maybe DocComment)
  , effectDefName        :: !Text
  , effectDefTypeParams  :: ![TypeParam]
  , effectDefOperations  :: ![Operation]
  , effectDefSpan        :: !Span
  } deriving stock (Eq, Show, Generic)

-- | An effect operation
data Operation = Operation
  { operationDocComment :: !(Maybe DocComment)
  , operationName       :: !Text
  , operationSignature  :: !Type
  , operationSpan       :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Handler definition
data HandlerDef = HandlerDef
  { handlerDefDocComment        :: !(Maybe DocComment)
  , handlerDefName              :: !Text
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
  { fnDefDocComment :: !(Maybe DocComment)
  , fnDefName       :: !Text
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

-- | Trait definition
-- Example: trait Ord A:
--            compare: (A, A) -> Ordering
data TraitDef = TraitDef
  { traitDefDocComment :: !(Maybe DocComment)
  , traitDefName       :: !Text
  , traitDefParam      :: !(Maybe Text)       -- ^ The type parameter (e.g., A in "trait Ord A"), Nothing for parameterless traits
  , traitDefParamKind  :: !(Maybe Kind)       -- ^ Optional kind annotation
  , traitDefSupers     :: ![TraitConstraint]  -- ^ Supertraits (e.g., Eq for Ord)
  , traitDefMethods    :: ![TraitMethod]
  , traitDefSpan       :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Style of trait method declaration (for formatter round-trip fidelity)
data TraitMethodStyle
  = TraitMethodSigStyle    -- ^ name: Type
  | TraitMethodFnStyle     -- ^ fn name(self, ...) -> Type
  deriving stock (Eq, Show, Generic)

-- | A method signature within a trait
data TraitMethod = TraitMethod
  { traitMethodName       :: !Text
  , traitMethodType       :: !Type
  , traitMethodDefault    :: !(Maybe Expr)      -- ^ Optional default implementation
  , traitMethodStyle      :: !TraitMethodStyle   -- ^ Original syntax style
  , traitMethodParams     :: ![Param]            -- ^ Parameters (for fn-style round-trip)
  , traitMethodReturnType :: !(Maybe Type)       -- ^ Return type (for fn-style round-trip)
  , traitMethodSpan       :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Implementation of a trait for a type
-- Example: impl Ord for Int:
--            compare(a, b): int_compare(a, b)
data ImplDef = ImplDef
  { implDefDocComment :: !(Maybe DocComment)
  , implDefTrait      :: !Text                 -- ^ Trait being implemented
  , implDefType       :: !Type                 -- ^ Type implementing the trait
  , implDefMethods    :: ![FunctionDef]        -- ^ Method implementations
  , implDefSpan       :: !Span
  } deriving stock (Eq, Show, Generic)

-- | A trait constraint (for bounded polymorphism)
-- Example: T: Ord or (T: Eq, T: Show)
data TraitConstraint = TraitConstraint
  { constraintTrait :: !Text
  , constraintType  :: !Type
  , constraintSpan  :: !Span
  } deriving stock (Eq, Show, Generic)

-- | A deriving clause on a type definition
-- Example: type Date deriving (Eq, Ord)
data DerivingClause = DerivingClause
  { derivingTraits :: ![Text]
  , derivingSpan   :: !Span
  } deriving stock (Eq, Show, Generic)

-- | External function reference for FFI
-- Example: @external("postgres", "query")
data ExternalRef = ExternalRef
  { externalModule   :: !Text           -- ^ Module/namespace (e.g., "postgres", "console")
  , externalFunction :: !Text           -- ^ Function name in external module
  , externalSpan     :: !Span
  } deriving stock (Eq, Show, Generic)

-- | External function definition (FFI binding)
-- Example: external fn query(sql: String) -> String = ("postgres", "query")
data ExternalFnDef = ExternalFnDef
  { extFnDefDocComment  :: !(Maybe DocComment)
  , extFnDefName        :: !Text
  , extFnDefParams      :: ![Param]
  , extFnDefReturnType  :: !Type
  , extFnDefExternal    :: !ExternalRef   -- ^ The external binding
  , extFnDefSpan        :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Type alias definition with optional field constraints
-- Example: type JudicialAuthority = Authority { action: Judicial(_) }
data TypeAliasDef = TypeAliasDef
  { typeAliasDocComment  :: !(Maybe DocComment)
  , typeAliasName        :: !Text           -- ^ Name of the alias (e.g., "JudicialAuthority")
  , typeAliasParams      :: ![TypeParam]    -- ^ Type parameters
  , typeAliasBase        :: !Type           -- ^ Base type (e.g., "Authority")
  , typeAliasConstraints :: ![FieldConstraint]  -- ^ Field constraints
  , typeAliasSpan        :: !Span
  } deriving stock (Eq, Show, Generic)

-- | A constraint on a field value in a type alias
-- Example: action: Judicial(_) or status: Active
data FieldConstraint = FieldConstraint
  { fieldConstraintName    :: !Text         -- ^ Field name being constrained
  , fieldConstraintPattern :: !Pattern      -- ^ Pattern the field must match
  , fieldConstraintSpan    :: !Span
  } deriving stock (Eq, Show, Generic)

-- | Comparison operators for refinement predicates
data ComparisonOp
  = OpLt           -- ^ <
  | OpLe           -- ^ <=
  | OpGt           -- ^ >
  | OpGe           -- ^ >=
  | OpEq           -- ^ ==
  | OpNe           -- ^ /=
  deriving stock (Eq, Show, Generic)

-- | A predicate in a refinement type
-- Example: 1 <= self, self <= 12, self > 0
data RefinementPredicate
  = RefinementComparison !Expr !ComparisonOp !Expr !Span
    -- ^ Comparison: left op right (e.g., 1 <= self)
  | RefinementAnd !RefinementPredicate !RefinementPredicate !Span
    -- ^ Conjunction: pred1 && pred2
  | RefinementOr !RefinementPredicate !RefinementPredicate !Span
    -- ^ Disjunction: pred1 || pred2
  | RefinementNot !RefinementPredicate !Span
    -- ^ Negation: !pred
  | RefinementExpr !Expr !Span
    -- ^ Arbitrary boolean expression
  deriving stock (Eq, Show, Generic)

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
  | TyRefinement !Type ![RefinementPredicate] !Span -- ^ Refinement type: T { predicate }
  | TyHole !Span                                    -- ^ Type hole (omitted annotation, to be inferred)
  deriving stock (Eq, Show, Generic)

-- | Style of lambda expression (for formatter round-trip fidelity)
data LambdaStyle
  = LamBackslash   -- ^ \\x: T. expr
  | LamFnArrow     -- ^ fn(x) -> expr
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
  | ELam !LambdaStyle ![Param] !Expr !Span               -- ^ Lambda expression
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
  | EExternal !ExternalRef ![Expr] !Span                -- ^ External function call
  | EFieldAccess !Expr !Text !Span                      -- ^ Field access: expr.field
  | EMethodCall !Expr !Text ![Expr] !Span               -- ^ Method call: expr.method(args)
  | EBinOp !BinOp !Expr !Expr !Span                     -- ^ Binary operator: expr op expr
  | ERecord !Text ![(Text, Expr)] !Span                 -- ^ Record construction: Type { field = expr, ... }
  | EFor !Pattern !Expr !Expr !Span                     -- ^ For loop: for pattern in collection: body
  | EList ![Expr] !Span                                 -- ^ List literal: [expr, ...]
  | EBreak !Span                                        -- ^ Break statement for loop exit
  | EReturn !Expr !Span                                 -- ^ Early return: return expr
  | EIndex !Expr !Expr !Span                            -- ^ Index access: expr[expr]
  | ERange !Expr !Expr !Span                            -- ^ Range expression: expr..expr
  | ETuple ![Expr] !Span                                -- ^ Tuple expression: (expr, expr, ...)
  | ENot !Expr !Span                                    -- ^ Boolean negation: not expr
  | EAssign !Text !Expr !Expr !Span                     -- ^ Mutable assignment: name = value; body
  deriving stock (Eq, Show, Generic)

-- | Binary operators
data BinOp
  = OpAdd         -- ^ +
  | OpSub         -- ^ -
  | OpMul         -- ^ *
  | OpDiv         -- ^ /
  | OpMod         -- ^ %
  | OpAnd         -- ^ &&
  | OpOr          -- ^ ||
  | OpLT          -- ^ <
  | OpLE          -- ^ <=
  | OpGT          -- ^ >
  | OpGE          -- ^ >=
  | OpEQ          -- ^ ==
  | OpNE          -- ^ /=
  | OpConcat      -- ^ ++
  | OpCons        -- ^ ::
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
