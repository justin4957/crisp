{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.IR.LLIRSpec
-- Description : Tests for Low-Level IR and monomorphization
--
-- TDD tests for the LLIR representation and monomorphization pass:
-- - LLIR representation and structure
-- - Monomorphization of polymorphic functions
-- - Data layout computation
-- - Closure representation
-- - Dead code elimination

module Crisp.IR.LLIRSpec (spec) where

import Test.Hspec

import Crisp.IR.LLIR
import Crisp.IR.Monomorphize
import Crisp.IR.Layout
import Crisp.Core.Term (Type(..), Kind(..), EffectRow(..))

import Data.Either (isRight, isLeft)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Helper to create simple types
tySimple :: T.Text -> Type
tySimple name = TyCon name []

tyInt :: Type
tyInt = tySimple "Int"

tyBool :: Type
tyBool = tySimple "Bool"

tyUnit :: Type
tyUnit = tySimple "Unit"

tyFloat :: Type
tyFloat = tySimple "Float"

tyString :: Type
tyString = tySimple "String"

-- | Type variable helper
tyVar :: T.Text -> Int -> Type
tyVar = TyVar

-- | Type constructor helper
tyCon :: T.Text -> [Type] -> Type
tyCon = TyCon

spec :: Spec
spec = do
  describe "LLIR" $ do
    llirTypeTests
    llirInstrTests
    llirFuncTests
    llirModuleTests
    monomorphizationTests
    specializedNamingTests
    dataLayoutTests
    closureRepresentationTests
    deadCodeEliminationTests
    recursivePolyTests
    edgeCaseTests

--------------------------------------------------------------------------------
-- LLIR Type Tests
--------------------------------------------------------------------------------

llirTypeTests :: Spec
llirTypeTests = describe "LLIR types" $ do
  it "represents primitive types" $ do
    llirTypeSize LlirI32 `shouldBe` 4
    llirTypeSize LlirI64 `shouldBe` 8
    llirTypeSize LlirF32 `shouldBe` 4
    llirTypeSize LlirF64 `shouldBe` 8

  it "represents pointer types" $ do
    llirTypeSize LlirPtr `shouldBe` 4  -- Wasm32 pointer

  it "represents struct types" $ do
    let structTy = LlirStruct [LlirI32, LlirI32]
    llirTypeSize structTy `shouldBe` 8
    llirTypeAlign structTy `shouldBe` 4

  it "computes struct alignment" $ do
    let structTy = LlirStruct [LlirI32, LlirI64]
    llirTypeAlign structTy `shouldBe` 8  -- Aligned to largest member

  it "represents tagged unions" $ do
    let unionTy = LlirTaggedUnion [LlirI32, LlirI64]
    llirTypeSize unionTy `shouldBe` 12  -- tag (4) + max payload (8)

  it "represents function types" $ do
    let fnTy = LlirFuncType [LlirI32, LlirI32] LlirI32
    llirFuncTypeArity fnTy `shouldBe` 2
    llirFuncTypeReturn fnTy `shouldBe` LlirI32

  it "converts Crisp Int to LlirI32" $ do
    toLlirType tyInt `shouldBe` LlirI32

  it "converts Crisp Bool to LlirI32" $ do
    toLlirType tyBool `shouldBe` LlirI32  -- Bools are i32 in Wasm

  it "converts Crisp Unit to LlirUnit" $ do
    toLlirType tyUnit `shouldBe` LlirUnit

  it "converts Crisp Float to LlirF64" $ do
    toLlirType tyFloat `shouldBe` LlirF64

--------------------------------------------------------------------------------
-- LLIR Instruction Tests
--------------------------------------------------------------------------------

llirInstrTests :: Spec
llirInstrTests = describe "LLIR instructions" $ do
  it "represents constant values" $ do
    let instr = LlirConst (LlirValI32 42)
    llirInstrIsConst instr `shouldBe` True

  it "represents local variable access" $ do
    let instr = LlirLocalGet 0
    llirInstrIsLocalGet instr `shouldBe` True

  it "represents local variable set" $ do
    let instr = LlirLocalSet 0
    llirInstrIsLocalSet instr `shouldBe` True

  it "represents memory load" $ do
    let instr = LlirLoad LlirI32 0
    llirInstrIsLoad instr `shouldBe` True

  it "represents memory store" $ do
    let instr = LlirStore LlirI32 0
    llirInstrIsStore instr `shouldBe` True

  it "represents function call" $ do
    let instr = LlirCall "add" 2
    llirInstrIsCall instr `shouldBe` True
    llirCallTarget instr `shouldBe` Just "add"

  it "represents indirect call" $ do
    let instr = LlirCallIndirect (LlirFuncType [LlirI32] LlirI32)
    llirInstrIsCallIndirect instr `shouldBe` True

  it "represents if-then-else" $ do
    let instr = LlirIf LlirI32 [LlirConst (LlirValI32 1)] [LlirConst (LlirValI32 0)]
    llirInstrIsIf instr `shouldBe` True

  it "represents blocks" $ do
    let instr = LlirBlock "label" [LlirConst (LlirValI32 42)]
    llirInstrIsBlock instr `shouldBe` True

  it "represents loops" $ do
    let instr = LlirLoop "loop" [LlirBr "loop"]
    llirInstrIsLoop instr `shouldBe` True

  it "represents branch" $ do
    let instr = LlirBr "label"
    llirInstrIsBr instr `shouldBe` True

  it "represents branch-if" $ do
    let instr = LlirBrIf "label"
    llirInstrIsBrIf instr `shouldBe` True

  it "represents return" $ do
    let instr = LlirReturn
    llirInstrIsReturn instr `shouldBe` True

  it "represents binary operations" $ do
    let instr = LlirBinOp LlirAdd
    llirInstrIsBinOp instr `shouldBe` True

  it "represents comparison operations" $ do
    let instr = LlirCmpOp LlirEq
    llirInstrIsCmpOp instr `shouldBe` True

--------------------------------------------------------------------------------
-- LLIR Function Tests
--------------------------------------------------------------------------------

llirFuncTests :: Spec
llirFuncTests = describe "LLIR functions" $ do
  it "represents monomorphic function" $ do
    let func = LlirFunc
          { llirFuncName = "add"
          , llirFuncParams = [("x", LlirI32), ("y", LlirI32)]
          , llirFuncReturn = LlirI32
          , llirFuncLocals = []
          , llirFuncBody = [LlirLocalGet 0, LlirLocalGet 1, LlirBinOp LlirAdd]
          }
    llirFuncArity func `shouldBe` 2
    llirFuncName func `shouldBe` "add"

  it "tracks local variables" $ do
    let func = LlirFunc
          { llirFuncName = "test"
          , llirFuncParams = [("x", LlirI32)]
          , llirFuncReturn = LlirI32
          , llirFuncLocals = [("tmp", LlirI32)]
          , llirFuncBody = []
          }
    llirFuncLocalCount func `shouldBe` 2  -- param + local

  it "computes stack frame size" $ do
    let func = LlirFunc
          { llirFuncName = "test"
          , llirFuncParams = [("a", LlirI32), ("b", LlirI64)]
          , llirFuncReturn = LlirI32
          , llirFuncLocals = [("c", LlirI32)]
          , llirFuncBody = []
          }
    llirFuncFrameSize func `shouldBe` 16  -- 4 + 8 + 4

  it "identifies exported functions" $ do
    let func = LlirFunc
          { llirFuncName = "main"
          , llirFuncParams = []
          , llirFuncReturn = LlirI32
          , llirFuncLocals = []
          , llirFuncBody = [LlirConst (LlirValI32 0), LlirReturn]
          }
    isExportedFunc "main" `shouldBe` True
    isExportedFunc "_internal" `shouldBe` False

--------------------------------------------------------------------------------
-- LLIR Module Tests
--------------------------------------------------------------------------------

llirModuleTests :: Spec
llirModuleTests = describe "LLIR modules" $ do
  it "collects all functions" $ do
    let mod' = LlirModule
          { llirModFunctions = [testFunc "f1", testFunc "f2"]
          , llirModTypes = []
          , llirModMemory = defaultMemory
          , llirModImports = []
          , llirModExports = []
          }
    length (llirModFunctions mod') `shouldBe` 2

  it "tracks type definitions" $ do
    let mod' = LlirModule
          { llirModFunctions = []
          , llirModTypes = [LlirTypeDef "Point" (LlirStruct [LlirI32, LlirI32])]
          , llirModMemory = defaultMemory
          , llirModImports = []
          , llirModExports = []
          }
    length (llirModTypes mod') `shouldBe` 1

  it "manages memory layout" $ do
    let mem = LlirMemory { llirMemPages = 1, llirMemMaxPages = Just 10 }
    llirMemPages mem `shouldBe` 1
    llirMemMaxPages mem `shouldBe` Just 10

  it "tracks imports" $ do
    let imp = LlirImport
          { llirImportModule = "env"
          , llirImportName = "print"
          , llirImportType = LlirImportFunc (LlirFuncType [LlirI32] LlirUnit)
          }
    llirImportModule imp `shouldBe` "env"

  it "tracks exports" $ do
    let exp' = LlirExport
          { llirExportName = "main"
          , llirExportKind = LlirExportFunc "main"
          }
    llirExportName exp' `shouldBe` "main"

--------------------------------------------------------------------------------
-- Monomorphization Tests
--------------------------------------------------------------------------------

monomorphizationTests :: Spec
monomorphizationTests = describe "monomorphization" $ do
  it "keeps monomorphic function unchanged" $ do
    let monoFunc = PolyFunc
          { polyFuncName = "add"
          , polyFuncTypeParams = []
          , polyFuncParams = [("x", tyInt), ("y", tyInt)]
          , polyFuncReturn = tyInt
          , polyFuncBody = MonoBody []  -- Simplified for testing
          }
    let result = monomorphize (MonoEnv [monoFunc] [])
    length (monoResultFuncs result) `shouldBe` 1
    llirFuncName (head (monoResultFuncs result)) `shouldBe` "add"

  it "specializes identity function for Int" $ do
    let idFunc = PolyFunc
          { polyFuncName = "id"
          , polyFuncTypeParams = [("A", KiType 0)]
          , polyFuncParams = [("x", tyVar "A" 0)]
          , polyFuncReturn = tyVar "A" 0
          , polyFuncBody = MonoBody []
          }
    let callSite = CallSite "id" [tyInt]
    let result = monomorphize (MonoEnv [idFunc] [callSite])
    let names = map llirFuncName (monoResultFuncs result)
    names `shouldContain` ["id$Int"]

  it "specializes identity function for Bool" $ do
    let idFunc = PolyFunc
          { polyFuncName = "id"
          , polyFuncTypeParams = [("A", KiType 0)]
          , polyFuncParams = [("x", tyVar "A" 0)]
          , polyFuncReturn = tyVar "A" 0
          , polyFuncBody = MonoBody []
          }
    let callSites = [CallSite "id" [tyInt], CallSite "id" [tyBool]]
    let result = monomorphize (MonoEnv [idFunc] callSites)
    let names = map llirFuncName (monoResultFuncs result)
    names `shouldContain` ["id$Int"]
    names `shouldContain` ["id$Bool"]

  it "specializes function with multiple type parameters" $ do
    let constFunc = PolyFunc
          { polyFuncName = "const"
          , polyFuncTypeParams = [("A", KiType 0), ("B", KiType 0)]
          , polyFuncParams = [("x", tyVar "A" 0), ("y", tyVar "B" 1)]
          , polyFuncReturn = tyVar "A" 0
          , polyFuncBody = MonoBody []
          }
    let callSite = CallSite "const" [tyInt, tyBool]
    let result = monomorphize (MonoEnv [constFunc] [callSite])
    let names = map llirFuncName (monoResultFuncs result)
    names `shouldContain` ["const$Int$Bool"]

  it "handles nested polymorphic calls" $ do
    -- id(id(42)) should generate id$Int
    let idFunc = PolyFunc
          { polyFuncName = "id"
          , polyFuncTypeParams = [("A", KiType 0)]
          , polyFuncParams = [("x", tyVar "A" 0)]
          , polyFuncReturn = tyVar "A" 0
          , polyFuncBody = MonoBody []
          }
    let callSite = CallSite "id" [tyInt]  -- Both calls use Int
    let result = monomorphize (MonoEnv [idFunc] [callSite])
    length (monoResultFuncs result) `shouldBe` 1

  it "substitutes types in function body" $ do
    let mapFunc = PolyFunc
          { polyFuncName = "map"
          , polyFuncTypeParams = [("A", KiType 0), ("B", KiType 0)]
          , polyFuncParams = [("f", TyPi "_" (tyVar "A" 0) EffEmpty (tyVar "B" 1))]
          , polyFuncReturn = tyCon "List" [tyVar "B" 1]
          , polyFuncBody = MonoBody []
          }
    let callSite = CallSite "map" [tyInt, tyBool]
    let result = monomorphize (MonoEnv [mapFunc] [callSite])
    -- Should produce map$Int$Bool with concrete types
    length (monoResultFuncs result) `shouldBe` 1

--------------------------------------------------------------------------------
-- Specialized Naming Tests
--------------------------------------------------------------------------------

specializedNamingTests :: Spec
specializedNamingTests = describe "specialized naming" $ do
  it "generates simple specialized name" $ do
    specializedName "id" [tyInt] `shouldBe` "id$Int"

  it "generates multi-param specialized name" $ do
    specializedName "const" [tyInt, tyBool] `shouldBe` "const$Int$Bool"

  it "handles parameterized types in names" $ do
    specializedName "head" [tyCon "List" [tyInt]] `shouldBe` "head$List_Int"

  it "handles nested parameterized types" $ do
    specializedName "f" [tyCon "List" [tyCon "Option" [tyInt]]]
      `shouldBe` "f$List_Option_Int"

  it "keeps monomorphic name unchanged" $ do
    specializedName "add" [] `shouldBe` "add"

  it "escapes special characters in type names" $ do
    -- Function types get hashed or simplified
    let fnType = TyPi "_" tyInt EffEmpty tyBool
    let name = specializedName "apply" [fnType]
    T.any (== '$') name `shouldBe` True

--------------------------------------------------------------------------------
-- Data Layout Tests
--------------------------------------------------------------------------------

dataLayoutTests :: Spec
dataLayoutTests = describe "data layout" $ do
  it "computes Int layout" $ do
    let layout = computeLayout tyInt
    layoutSize layout `shouldBe` 4
    layoutAlign layout `shouldBe` 4

  it "computes Bool layout" $ do
    let layout = computeLayout tyBool
    layoutSize layout `shouldBe` 4  -- Represented as i32
    layoutAlign layout `shouldBe` 4

  it "computes struct layout" $ do
    -- type Point = Point(x: Int, y: Int)
    let pointDef = TypeDef
          { typeDefName = "Point"
          , typeDefParams = []
          , typeDefConstrs = [ConstrDef "Point" [tyInt, tyInt] (tyCon "Point" [])]
          }
    let layout = computeTypeDefLayout pointDef
    layoutSize layout `shouldBe` 8
    layoutAlign layout `shouldBe` 4

  it "computes tagged union layout" $ do
    -- type Option(A) = None | Some(A)
    let optionDef = TypeDef
          { typeDefName = "Option"
          , typeDefParams = [("A", KiType 0)]
          , typeDefConstrs =
              [ ConstrDef "None" [] (tyCon "Option" [tyVar "A" 0])
              , ConstrDef "Some" [tyVar "A" 0] (tyCon "Option" [tyVar "A" 0])
              ]
          }
    let layout = computeTypeDefLayout optionDef
    layoutTag layout `shouldBe` Just 4  -- Tag is 4 bytes

  it "computes field offsets" $ do
    let pointDef = TypeDef
          { typeDefName = "Point"
          , typeDefParams = []
          , typeDefConstrs = [ConstrDef "Point" [tyInt, tyInt] (tyCon "Point" [])]
          }
    let layout = computeTypeDefLayout pointDef
    layoutFieldOffsets layout `shouldBe` [0, 4]

  it "handles alignment padding" $ do
    -- type Mixed = Mixed(a: Bool, b: Int64)
    let mixedDef = TypeDef
          { typeDefName = "Mixed"
          , typeDefParams = []
          , typeDefConstrs = [ConstrDef "Mixed" [tyBool, tyCon "Int64" []] (tyCon "Mixed" [])]
          }
    let layout = computeTypeDefLayout mixedDef
    layoutAlign layout `shouldBe` 8  -- Aligned to Int64
    -- Bool at 0, padding at 4-7, Int64 at 8
    layoutFieldOffsets layout `shouldBe` [0, 8]
    layoutSize layout `shouldBe` 16

  it "computes recursive type layout" $ do
    -- type List(A) = Nil | Cons(head: A, tail: List(A))
    let listDef = TypeDef
          { typeDefName = "List"
          , typeDefParams = [("A", KiType 0)]
          , typeDefConstrs =
              [ ConstrDef "Nil" [] (tyCon "List" [tyVar "A" 0])
              , ConstrDef "Cons" [tyVar "A" 0, tyCon "List" [tyVar "A" 0]] (tyCon "List" [tyVar "A" 0])
              ]
          }
    let layout = computeTypeDefLayout listDef
    -- Recursive types use pointers, so Cons is (tag, value, pointer)
    layoutTag layout `shouldBe` Just 4

  it "computes empty struct layout" $ do
    let emptyDef = TypeDef
          { typeDefName = "Empty"
          , typeDefParams = []
          , typeDefConstrs = [ConstrDef "Empty" [] (tyCon "Empty" [])]
          }
    let layout = computeTypeDefLayout emptyDef
    layoutSize layout `shouldBe` 0  -- No fields

--------------------------------------------------------------------------------
-- Closure Representation Tests
--------------------------------------------------------------------------------

closureRepresentationTests :: Spec
closureRepresentationTests = describe "closure representation" $ do
  it "represents closure with no captures" $ do
    let closure = ClosureRep
          { closureFuncPtr = "lambda_0"
          , closureCaptures = []
          }
    closureCaptureCount closure `shouldBe` 0

  it "represents closure with captured variable" $ do
    let closure = ClosureRep
          { closureFuncPtr = "make_adder$lambda"
          , closureCaptures = [("n", LlirI32)]
          }
    closureCaptureCount closure `shouldBe` 1

  it "computes closure struct size" $ do
    let closure = ClosureRep
          { closureFuncPtr = "f"
          , closureCaptures = [("x", LlirI32), ("y", LlirI32)]
          }
    closureSize closure `shouldBe` 12  -- ptr (4) + x (4) + y (4)

  it "generates closure allocation code" $ do
    let closure = ClosureRep
          { closureFuncPtr = "f"
          , closureCaptures = [("n", LlirI32)]
          }
    let instrs = generateClosureAlloc closure
    -- Should allocate, store func ptr, store captures
    length instrs `shouldSatisfy` (> 0)
    any llirInstrIsCall instrs `shouldBe` True  -- alloc call

  it "generates closure invocation code" $ do
    let closure = ClosureRep
          { closureFuncPtr = "f"
          , closureCaptures = [("n", LlirI32)]
          }
    let instrs = generateClosureCall closure
    -- Should load func ptr and do indirect call
    any llirInstrIsCallIndirect instrs `shouldBe` True

  it "handles nested closures" $ do
    -- fn outer(x) = fn inner(y) = x + y
    let innerClosure = ClosureRep
          { closureFuncPtr = "inner"
          , closureCaptures = [("x", LlirI32)]
          }
    let outerClosure = ClosureRep
          { closureFuncPtr = "outer"
          , closureCaptures = []
          }
    closureCaptureCount outerClosure `shouldBe` 0
    closureCaptureCount innerClosure `shouldBe` 1

--------------------------------------------------------------------------------
-- Dead Code Elimination Tests
--------------------------------------------------------------------------------

deadCodeEliminationTests :: Spec
deadCodeEliminationTests = describe "dead code elimination" $ do
  it "keeps exported functions" $ do
    let funcs = [testFunc "main", testFunc "unused"]
    let exports = Set.singleton "main"
    let result = eliminateDeadCode funcs exports
    map llirFuncName result `shouldBe` ["main"]

  it "keeps functions called by exports" $ do
    let mainFunc = testFuncWithCalls "main" ["helper"]
    let helperFunc = testFunc "helper"
    let unusedFunc = testFunc "unused"
    let exports = Set.singleton "main"
    let result = eliminateDeadCode [mainFunc, helperFunc, unusedFunc] exports
    let names = map llirFuncName result
    names `shouldContain` ["main"]
    names `shouldContain` ["helper"]
    names `shouldNotContain` ["unused"]

  it "keeps transitively called functions" $ do
    let f1 = testFuncWithCalls "main" ["f2"]
    let f2 = testFuncWithCalls "f2" ["f3"]
    let f3 = testFunc "f3"
    let unused = testFunc "unused"
    let exports = Set.singleton "main"
    let result = eliminateDeadCode [f1, f2, f3, unused] exports
    let names = map llirFuncName result
    length names `shouldBe` 3
    names `shouldContain` ["f3"]

  it "handles cyclic call graphs" $ do
    let f1 = testFuncWithCalls "f1" ["f2"]
    let f2 = testFuncWithCalls "f2" ["f1"]
    let exports = Set.singleton "f1"
    let result = eliminateDeadCode [f1, f2] exports
    length result `shouldBe` 2

  it "eliminates unused specializations" $ do
    let idInt = testFunc "id$Int"
    let idBool = testFunc "id$Bool"
    let mainFunc = testFuncWithCalls "main" ["id$Int"]
    let exports = Set.singleton "main"
    let result = eliminateDeadCode [mainFunc, idInt, idBool] exports
    let names = map llirFuncName result
    names `shouldContain` ["id$Int"]
    names `shouldNotContain` ["id$Bool"]

--------------------------------------------------------------------------------
-- Recursive Polymorphism Tests
--------------------------------------------------------------------------------

recursivePolyTests :: Spec
recursivePolyTests = describe "recursive polymorphism" $ do
  it "handles simple recursive function" $ do
    -- fn length(xs: List(A)) -> Int = match xs ...
    let lengthFunc = PolyFunc
          { polyFuncName = "length"
          , polyFuncTypeParams = [("A", KiType 0)]
          , polyFuncParams = [("xs", tyCon "List" [tyVar "A" 0])]
          , polyFuncReturn = tyInt
          , polyFuncBody = MonoBody []
          }
    let callSite = CallSite "length" [tyInt]
    let result = monomorphize (MonoEnv [lengthFunc] [callSite])
    length (monoResultFuncs result) `shouldBe` 1

  it "detects polymorphic recursion cycle" $ do
    -- fn f(x: List(A)) = f(wrap(x))  -- calls f with List(List(A))
    let cycleFunc = PolyFunc
          { polyFuncName = "f"
          , polyFuncTypeParams = [("A", KiType 0)]
          , polyFuncParams = [("x", tyCon "List" [tyVar "A" 0])]
          , polyFuncReturn = tyUnit
          , polyFuncBody = MonoBodyWithCalls [("f", [tyCon "List" [tyVar "A" 0]])]
          }
    let callSite = CallSite "f" [tyInt]
    let result = monomorphize (MonoEnv [cycleFunc] [callSite])
    -- Should detect cycle and limit instantiation depth
    monoResultHasCycleWarning result `shouldBe` True

  it "handles mutually recursive functions" $ do
    let evenFunc = PolyFunc
          { polyFuncName = "even"
          , polyFuncTypeParams = []
          , polyFuncParams = [("n", tyInt)]
          , polyFuncReturn = tyBool
          , polyFuncBody = MonoBodyWithCalls [("odd", [])]
          }
    let oddFunc = PolyFunc
          { polyFuncName = "odd"
          , polyFuncTypeParams = []
          , polyFuncParams = [("n", tyInt)]
          , polyFuncReturn = tyBool
          , polyFuncBody = MonoBodyWithCalls [("even", [])]
          }
    let callSite = CallSite "even" []
    let result = monomorphize (MonoEnv [evenFunc, oddFunc] [callSite])
    let names = map llirFuncName (monoResultFuncs result)
    names `shouldContain` ["even"]
    names `shouldContain` ["odd"]

  it "limits instantiation depth for nested types" $ do
    -- Prevent infinite instantiation chains like List(List(List(...)))
    let deepFunc = PolyFunc
          { polyFuncName = "deep"
          , polyFuncTypeParams = [("A", KiType 0)]
          , polyFuncParams = [("x", tyVar "A" 0)]
          , polyFuncReturn = tyUnit
          , polyFuncBody = MonoBodyWithCalls [("deep", [tyCon "List" [tyVar "A" 0]])]
          }
    let callSite = CallSite "deep" [tyInt]
    let result = monomorphizeWithDepth 3 (MonoEnv [deepFunc] [callSite])
    -- Should stop at depth 3
    length (monoResultFuncs result) `shouldSatisfy` (<= 4)

--------------------------------------------------------------------------------
-- Edge Case Tests
--------------------------------------------------------------------------------

edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles empty module" $ do
    let result = monomorphize (MonoEnv [] [])
    monoResultFuncs result `shouldBe` []

  it "handles function with no parameters" $ do
    let constFunc = PolyFunc
          { polyFuncName = "fortytwo"
          , polyFuncTypeParams = []
          , polyFuncParams = []
          , polyFuncReturn = tyInt
          , polyFuncBody = MonoBody []
          }
    let result = monomorphize (MonoEnv [constFunc] [CallSite "fortytwo" []])
    length (monoResultFuncs result) `shouldBe` 1

  it "handles higher-kinded type parameters" $ do
    -- fn fmap(f: A -> B, fa: F(A)) -> F(B)
    let fmapFunc = PolyFunc
          { polyFuncName = "fmap"
          , polyFuncTypeParams = [("F", KiArrow (KiType 0) (KiType 0)), ("A", KiType 0), ("B", KiType 0)]
          , polyFuncParams = []
          , polyFuncReturn = tyUnit  -- Simplified
          , polyFuncBody = MonoBody []
          }
    -- Higher-kinded types need special handling
    let result = monomorphize (MonoEnv [fmapFunc] [])
    -- Should produce no output since no call sites with concrete types
    monoResultFuncs result `shouldBe` []

  it "preserves function order" $ do
    let funcs = [testFunc "f1", testFunc "f2", testFunc "f3"]
    let result = monomorphize (MonoEnv (map toPolyFunc funcs) (map (`CallSite` []) ["f1", "f2", "f3"]))
    map llirFuncName (monoResultFuncs result) `shouldBe` ["f1", "f2", "f3"]

  it "handles unit return type" $ do
    let voidFunc = PolyFunc
          { polyFuncName = "noop"
          , polyFuncTypeParams = []
          , polyFuncParams = []
          , polyFuncReturn = tyUnit
          , polyFuncBody = MonoBody []
          }
    let result = monomorphize (MonoEnv [voidFunc] [CallSite "noop" []])
    llirFuncReturn (head (monoResultFuncs result)) `shouldBe` LlirUnit

  it "handles string type" $ do
    let strFunc = PolyFunc
          { polyFuncName = "greet"
          , polyFuncTypeParams = []
          , polyFuncParams = [("name", tyString)]
          , polyFuncReturn = tyString
          , polyFuncBody = MonoBody []
          }
    let result = monomorphize (MonoEnv [strFunc] [CallSite "greet" []])
    let func = head (monoResultFuncs result)
    snd (head (llirFuncParams func)) `shouldBe` LlirPtr  -- Strings are pointers

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Create a simple test function
testFunc :: T.Text -> LlirFunc
testFunc name = LlirFunc
  { llirFuncName = name
  , llirFuncParams = []
  , llirFuncReturn = LlirI32
  , llirFuncLocals = []
  , llirFuncBody = [LlirConst (LlirValI32 0), LlirReturn]
  }

-- | Create a test function with calls to other functions
testFuncWithCalls :: T.Text -> [T.Text] -> LlirFunc
testFuncWithCalls name calls = LlirFunc
  { llirFuncName = name
  , llirFuncParams = []
  , llirFuncReturn = LlirI32
  , llirFuncLocals = []
  , llirFuncBody = map (`LlirCall` 0) calls ++ [LlirConst (LlirValI32 0), LlirReturn]
  }

-- | Convert test func to poly func for monomorphization
toPolyFunc :: LlirFunc -> PolyFunc
toPolyFunc f = PolyFunc
  { polyFuncName = llirFuncName f
  , polyFuncTypeParams = []
  , polyFuncParams = map (\(n, t) -> (n, fromLlirType t)) (llirFuncParams f)
  , polyFuncReturn = fromLlirType (llirFuncReturn f)
  , polyFuncBody = MonoBody []
  }

-- | Default memory configuration
defaultMemory :: LlirMemory
defaultMemory = LlirMemory { llirMemPages = 1, llirMemMaxPages = Nothing }
