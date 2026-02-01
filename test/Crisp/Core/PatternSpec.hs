{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Core.PatternSpec
-- Description : Tests for pattern match elaboration
--
-- TDD tests for transforming surface-level pattern matching into
-- core calculus case expressions with explicit constructor checking.

module Crisp.Core.PatternSpec (spec) where

import Test.Hspec

import Crisp.Core.PatternCompiler
import Crisp.Core.Term as C
import Crisp.Syntax.Surface as S
import Crisp.Syntax.Span (Span(..), Position(..))

import Data.Either (isRight, isLeft)
import qualified Data.Text as T

-- | Helper to create a dummy span for testing
dummySpan :: Span
dummySpan = Span
  { spanStart = Position 1 1
  , spanEnd = Position 1 1
  , spanFile = "test"
  }

-- | Helper to create a surface variable expression
sVar :: T.Text -> S.Expr
sVar name = S.EVar name dummySpan

-- | Helper to create a surface constructor expression
sCon :: T.Text -> S.Expr
sCon name = S.ECon name dummySpan

-- | Helper to create a surface application
sApp :: S.Expr -> [S.Expr] -> S.Expr
sApp f args = S.EApp f args dummySpan

-- | Helper to create a surface match expression
sMatch :: S.Expr -> [S.MatchArm] -> S.Expr
sMatch subject arms = S.EMatch subject arms dummySpan

-- | Helper to create a match arm
sArm :: S.Pattern -> S.Expr -> S.MatchArm
sArm pat body = S.MatchArm
  { S.matchArmPattern = pat
  , S.matchArmGuard = Nothing
  , S.matchArmBody = body
  , S.matchArmSpan = dummySpan
  }

-- | Helper to create a match arm with guard
sArmGuarded :: S.Pattern -> S.Expr -> S.Expr -> S.MatchArm
sArmGuarded pat guard body = S.MatchArm
  { S.matchArmPattern = pat
  , S.matchArmGuard = Just guard
  , S.matchArmBody = body
  , S.matchArmSpan = dummySpan
  }

-- | Helper to create a variable pattern
pVar :: T.Text -> S.Pattern
pVar name = S.PatVar name dummySpan

-- | Helper to create a wildcard pattern
pWild :: S.Pattern
pWild = S.PatWildcard dummySpan

-- | Helper to create a constructor pattern
pCon :: T.Text -> [S.Pattern] -> S.Pattern
pCon name pats = S.PatCon name pats dummySpan

-- | Helper to create a tuple pattern
pTuple :: [S.Pattern] -> S.Pattern
pTuple pats = S.PatTuple pats dummySpan

-- | Helper to create a literal pattern
pLit :: S.Expr -> S.Pattern
pLit expr = S.PatLit expr dummySpan

-- | Helper to create an integer literal expression
sInt :: Integer -> S.Expr
sInt n = S.EIntLit n dummySpan

-- | Check if result is a successful elaboration
shouldElaborate :: Either PatternError C.Term -> Expectation
shouldElaborate result = result `shouldSatisfy` isRight

-- | Check if result fails elaboration
shouldFailElaboration :: Either PatternError C.Term -> Expectation
shouldFailElaboration result = result `shouldSatisfy` isLeft

spec :: Spec
spec = do
  describe "Simple Pattern Elaboration" $ do
    simplePatternTests
    wildcardPatternTests
    variablePatternTests

  describe "Constructor Pattern Elaboration" $ do
    constructorPatternTests
    nestedConstructorPatternTests

  describe "Tuple Pattern Elaboration" $ do
    tuplePatternTests

  describe "Literal Pattern Elaboration" $ do
    literalPatternTests

  describe "Guard Elaboration" $ do
    guardTests

  describe "Multiple Clauses" $ do
    multiClauseTests

  describe "Pattern Variable Scoping" $ do
    scopingTests

  describe "Edge Cases" $ do
    edgeCaseTests

-- | Tests for simple patterns (variable, wildcard)
simplePatternTests :: Spec
simplePatternTests = describe "simple patterns" $ do
  it "elaborates variable pattern to binding" $ do
    -- match x: y -> y
    -- should become: let y = x in y
    let subject = sVar "x"
        arm = sArm (pVar "y") (sVar "y")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "elaborates single variable pattern preserving variable name" $ do
    let subject = sVar "input"
        arm = sArm (pVar "result") (sVar "result")
        matchExpr = sMatch subject [arm]
    case elaborateMatch matchExpr of
      Right (C.TmMatch _ _ [C.Case (C.PatVar name) _]) ->
        name `shouldBe` "result"
      Right other -> expectationFailure $ "Unexpected structure: " ++ show other
      Left err -> expectationFailure $ "Elaboration failed: " ++ show err

-- | Tests for wildcard patterns
wildcardPatternTests :: Spec
wildcardPatternTests = describe "wildcard patterns" $ do
  it "elaborates wildcard pattern" $ do
    -- match x: _ -> 0
    let subject = sVar "x"
        arm = sArm pWild (sInt 0)
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "wildcard pattern creates PatWild" $ do
    let subject = sVar "x"
        arm = sArm pWild (sInt 42)
        matchExpr = sMatch subject [arm]
    case elaborateMatch matchExpr of
      Right (C.TmMatch _ _ [C.Case C.PatWild _]) -> pure ()
      Right other -> expectationFailure $ "Expected PatWild, got: " ++ show other
      Left err -> expectationFailure $ "Elaboration failed: " ++ show err

  it "elaborates multiple wildcards in sequence" $ do
    let subject = sVar "x"
        arm1 = sArm pWild (sInt 1)
        arm2 = sArm pWild (sInt 2)
        matchExpr = sMatch subject [arm1, arm2]
    shouldElaborate $ elaborateMatch matchExpr

-- | Tests for variable patterns
variablePatternTests :: Spec
variablePatternTests = describe "variable patterns" $ do
  it "variable pattern binds the matched value" $ do
    -- match x: y -> y should have y bound to x's value
    let subject = sVar "x"
        arm = sArm (pVar "y") (sVar "y")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "variable pattern with different body expression" $ do
    -- match x: y -> f y
    let subject = sVar "x"
        body = sApp (sVar "f") [sVar "y"]
        arm = sArm (pVar "y") body
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

-- | Tests for constructor patterns
constructorPatternTests :: Spec
constructorPatternTests = describe "constructor patterns" $ do
  it "elaborates nullary constructor pattern" $ do
    -- match x: None -> 0
    let subject = sVar "x"
        arm = sArm (pCon "None" []) (sInt 0)
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "nullary constructor creates PatCon with empty subpatterns" $ do
    let subject = sVar "x"
        arm = sArm (pCon "None" []) (sInt 0)
        matchExpr = sMatch subject [arm]
    case elaborateMatch matchExpr of
      Right (C.TmMatch _ _ [C.Case (C.PatCon "None" []) _]) -> pure ()
      Right other -> expectationFailure $ "Expected PatCon None [], got: " ++ show other
      Left err -> expectationFailure $ "Elaboration failed: " ++ show err

  it "elaborates unary constructor pattern" $ do
    -- match x: Some(y) -> y
    let subject = sVar "x"
        arm = sArm (pCon "Some" [pVar "y"]) (sVar "y")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "unary constructor binds its argument" $ do
    let subject = sVar "x"
        arm = sArm (pCon "Just" [pVar "value"]) (sVar "value")
        matchExpr = sMatch subject [arm]
    case elaborateMatch matchExpr of
      Right (C.TmMatch _ _ [C.Case (C.PatCon "Just" [C.PatVar "value"]) _]) -> pure ()
      Right other -> expectationFailure $ "Expected Just pattern, got: " ++ show other
      Left err -> expectationFailure $ "Elaboration failed: " ++ show err

  it "elaborates binary constructor pattern" $ do
    -- match xs: Cons(h, t) -> h
    let subject = sVar "xs"
        arm = sArm (pCon "Cons" [pVar "h", pVar "t"]) (sVar "h")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "binary constructor creates PatCon with two subpatterns" $ do
    let subject = sVar "xs"
        arm = sArm (pCon "Cons" [pVar "head", pVar "tail"]) (sVar "head")
        matchExpr = sMatch subject [arm]
    case elaborateMatch matchExpr of
      Right (C.TmMatch _ _ [C.Case (C.PatCon "Cons" [C.PatVar "head", C.PatVar "tail"]) _]) ->
        pure ()
      Right other -> expectationFailure $ "Expected Cons pattern, got: " ++ show other
      Left err -> expectationFailure $ "Elaboration failed: " ++ show err

  it "elaborates constructor pattern with wildcard subpattern" $ do
    -- match x: Some(_) -> 1
    let subject = sVar "x"
        arm = sArm (pCon "Some" [pWild]) (sInt 1)
        matchExpr = sMatch subject [arm]
    case elaborateMatch matchExpr of
      Right (C.TmMatch _ _ [C.Case (C.PatCon "Some" [C.PatWild]) _]) -> pure ()
      Right other -> expectationFailure $ "Expected Some(_), got: " ++ show other
      Left err -> expectationFailure $ "Elaboration failed: " ++ show err

-- | Tests for nested constructor patterns
nestedConstructorPatternTests :: Spec
nestedConstructorPatternTests = describe "nested constructor patterns" $ do
  it "elaborates singly nested pattern" $ do
    -- match x: Some(Some(y)) -> y
    let subject = sVar "x"
        innerPat = pCon "Some" [pVar "y"]
        outerPat = pCon "Some" [innerPat]
        arm = sArm outerPat (sVar "y")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "nested pattern structure is preserved" $ do
    let subject = sVar "x"
        innerPat = pCon "Just" [pVar "inner"]
        outerPat = pCon "Just" [innerPat]
        arm = sArm outerPat (sVar "inner")
        matchExpr = sMatch subject [arm]
    case elaborateMatch matchExpr of
      Right (C.TmMatch _ _ [C.Case (C.PatCon "Just" [C.PatCon "Just" [C.PatVar "inner"]]) _]) ->
        pure ()
      Right other -> expectationFailure $ "Expected nested Just, got: " ++ show other
      Left err -> expectationFailure $ "Elaboration failed: " ++ show err

  it "elaborates deeply nested pattern" $ do
    -- match x: Cons(Some(y), xs) -> y
    let subject = sVar "x"
        innerPat = pCon "Some" [pVar "y"]
        outerPat = pCon "Cons" [innerPat, pVar "xs"]
        arm = sArm outerPat (sVar "y")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "elaborates pattern with multiple nested constructors" $ do
    -- match p: Pair(Some(a), Some(b)) -> a
    let subject = sVar "p"
        pat1 = pCon "Some" [pVar "a"]
        pat2 = pCon "Some" [pVar "b"]
        outerPat = pCon "Pair" [pat1, pat2]
        arm = sArm outerPat (sVar "a")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

-- | Tests for tuple patterns
tuplePatternTests :: Spec
tuplePatternTests = describe "tuple patterns" $ do
  it "elaborates pair pattern" $ do
    -- match p: (a, b) -> a
    let subject = sVar "p"
        arm = sArm (pTuple [pVar "a", pVar "b"]) (sVar "a")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "tuple pattern becomes PatCon Tuple" $ do
    let subject = sVar "p"
        arm = sArm (pTuple [pVar "x", pVar "y"]) (sVar "x")
        matchExpr = sMatch subject [arm]
    case elaborateMatch matchExpr of
      Right (C.TmMatch _ _ [C.Case (C.PatCon "Tuple" [C.PatVar "x", C.PatVar "y"]) _]) ->
        pure ()
      Right other -> expectationFailure $ "Expected Tuple pattern, got: " ++ show other
      Left err -> expectationFailure $ "Elaboration failed: " ++ show err

  it "elaborates triple pattern" $ do
    let subject = sVar "t"
        arm = sArm (pTuple [pVar "a", pVar "b", pVar "c"]) (sVar "a")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "elaborates nested tuple pattern" $ do
    -- match p: ((a, b), c) -> a
    let subject = sVar "p"
        innerPat = pTuple [pVar "a", pVar "b"]
        outerPat = pTuple [innerPat, pVar "c"]
        arm = sArm outerPat (sVar "a")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "elaborates tuple with constructor inside" $ do
    -- match p: (Some(x), y) -> x
    let subject = sVar "p"
        pat = pTuple [pCon "Some" [pVar "x"], pVar "y"]
        arm = sArm pat (sVar "x")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

-- | Tests for literal patterns
literalPatternTests :: Spec
literalPatternTests = describe "literal patterns" $ do
  it "elaborates integer literal pattern" $ do
    -- match n: 0 -> "zero"
    let subject = sVar "n"
        arm = sArm (pLit (sInt 0)) (S.EStringLit S.StringSingle "zero" dummySpan)
        matchExpr = sMatch subject [arm]
    -- Literal patterns compile to equality checks
    shouldElaborate $ elaborateMatch matchExpr

  it "elaborates multiple literal alternatives" $ do
    -- match n: 0 -> "zero", 1 -> "one"
    let subject = sVar "n"
        arm1 = sArm (pLit (sInt 0)) (S.EStringLit S.StringSingle "zero" dummySpan)
        arm2 = sArm (pLit (sInt 1)) (S.EStringLit S.StringSingle "one" dummySpan)
        matchExpr = sMatch subject [arm1, arm2]
    shouldElaborate $ elaborateMatch matchExpr

-- | Tests for guard expressions
guardTests :: Spec
guardTests = describe "guard expressions" $ do
  it "elaborates simple guard" $ do
    -- match x: y if y > 0 -> y
    let subject = sVar "x"
        guard = sApp (sVar ">") [sVar "y", sInt 0]
        arm = sArmGuarded (pVar "y") guard (sVar "y")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "guard compiles to nested conditional" $ do
    let subject = sVar "x"
        guard = sApp (sVar "positive") [sVar "n"]
        arm = sArmGuarded (pVar "n") guard (sVar "n")
        matchExpr = sMatch subject [arm]
    case elaborateMatch matchExpr of
      Right term ->
        -- The result should contain a conditional structure
        -- We just verify it elaborates successfully for now
        pure ()
      Left err -> expectationFailure $ "Elaboration failed: " ++ show err

  it "elaborates guard with constructor pattern" $ do
    -- match opt: Some(x) if x > 0 -> x
    let subject = sVar "opt"
        guard = sApp (sVar ">") [sVar "x", sInt 0]
        arm = sArmGuarded (pCon "Some" [pVar "x"]) guard (sVar "x")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

-- | Tests for multiple match clauses
multiClauseTests :: Spec
multiClauseTests = describe "multiple clauses" $ do
  it "elaborates two clauses" $ do
    -- match opt: Some(x) -> x, None -> 0
    let subject = sVar "opt"
        arm1 = sArm (pCon "Some" [pVar "x"]) (sVar "x")
        arm2 = sArm (pCon "None" []) (sInt 0)
        matchExpr = sMatch subject [arm1, arm2]
    shouldElaborate $ elaborateMatch matchExpr

  it "preserves clause order" $ do
    let subject = sVar "opt"
        arm1 = sArm (pCon "Some" [pVar "x"]) (sVar "x")
        arm2 = sArm (pCon "None" []) (sInt 0)
        matchExpr = sMatch subject [arm1, arm2]
    case elaborateMatch matchExpr of
      Right (C.TmMatch _ _ cases) -> do
        length cases `shouldBe` 2
        case cases of
          [C.Case (C.PatCon "Some" _) _, C.Case (C.PatCon "None" _) _] -> pure ()
          _ -> expectationFailure "Clauses in wrong order"
      Right other -> expectationFailure $ "Expected TmMatch, got: " ++ show other
      Left err -> expectationFailure $ "Elaboration failed: " ++ show err

  it "elaborates multiple constructor alternatives" $ do
    -- match color: Red -> 1, Green -> 2, Blue -> 3
    let subject = sVar "color"
        arm1 = sArm (pCon "Red" []) (sInt 1)
        arm2 = sArm (pCon "Green" []) (sInt 2)
        arm3 = sArm (pCon "Blue" []) (sInt 3)
        matchExpr = sMatch subject [arm1, arm2, arm3]
    case elaborateMatch matchExpr of
      Right (C.TmMatch _ _ cases) -> length cases `shouldBe` 3
      Right other -> expectationFailure $ "Expected TmMatch, got: " ++ show other
      Left err -> expectationFailure $ "Elaboration failed: " ++ show err

  it "elaborates mixed pattern types" $ do
    -- match x: Some(y) -> y, _ -> 0
    let subject = sVar "x"
        arm1 = sArm (pCon "Some" [pVar "y"]) (sVar "y")
        arm2 = sArm pWild (sInt 0)
        matchExpr = sMatch subject [arm1, arm2]
    shouldElaborate $ elaborateMatch matchExpr

-- | Tests for pattern variable scoping
scopingTests :: Spec
scopingTests = describe "pattern variable scoping" $ do
  it "pattern variables are scoped to their branch" $ do
    -- match opt: Some(x) -> x, None -> y
    -- x should only be in scope in first branch
    let subject = sVar "opt"
        arm1 = sArm (pCon "Some" [pVar "x"]) (sVar "x")
        arm2 = sArm (pCon "None" []) (sVar "y")  -- y is a different var
        matchExpr = sMatch subject [arm1, arm2]
    shouldElaborate $ elaborateMatch matchExpr

  it "nested patterns bind all variables" $ do
    -- match p: Pair(Some(a), Some(b)) -> (a, b)
    let subject = sVar "p"
        pat = pCon "Pair" [pCon "Some" [pVar "a"], pCon "Some" [pVar "b"]]
        body = S.EApp (sCon "Pair") [sVar "a", sVar "b"] dummySpan
        arm = sArm pat body
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "duplicate variable names in separate branches are independent" $ do
    -- match x: Left(x) -> x, Right(x) -> x
    let subject = sVar "input"
        arm1 = sArm (pCon "Left" [pVar "x"]) (sVar "x")
        arm2 = sArm (pCon "Right" [pVar "x"]) (sVar "x")
        matchExpr = sMatch subject [arm1, arm2]
    shouldElaborate $ elaborateMatch matchExpr

-- | Edge case tests
edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles empty match arms" $ do
    -- match x: (no arms) - should still elaborate (though possibly to error)
    let subject = sVar "x"
        matchExpr = sMatch subject []
    -- Empty match is valid (though may produce match failure at runtime)
    shouldElaborate $ elaborateMatch matchExpr

  it "handles single variable pattern efficiently" $ do
    -- match x: y -> y should optimize to just a let binding
    let subject = sVar "x"
        arm = sArm (pVar "y") (sVar "y")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "handles deeply nested structures" $ do
    -- match x: Foo(Bar(Baz(Qux(y)))) -> y
    let subject = sVar "x"
        pat = pCon "Foo" [pCon "Bar" [pCon "Baz" [pCon "Qux" [pVar "y"]]]]
        arm = sArm pat (sVar "y")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "handles pattern with many variables" $ do
    -- match t: T(a, b, c, d, e) -> a
    let subject = sVar "t"
        pat = pCon "T" [pVar "a", pVar "b", pVar "c", pVar "d", pVar "e"]
        arm = sArm pat (sVar "a")
        matchExpr = sMatch subject [arm]
    shouldElaborate $ elaborateMatch matchExpr

  it "handles constructor with mixed variable and wildcard subpatterns" $ do
    -- match p: Pair(x, _) -> x
    let subject = sVar "p"
        arm = sArm (pCon "Pair" [pVar "x", pWild]) (sVar "x")
        matchExpr = sMatch subject [arm]
    case elaborateMatch matchExpr of
      Right (C.TmMatch _ _ [C.Case (C.PatCon "Pair" [C.PatVar "x", C.PatWild]) _]) ->
        pure ()
      Right other -> expectationFailure $ "Expected Pair(x, _), got: " ++ show other
      Left err -> expectationFailure $ "Elaboration failed: " ++ show err
