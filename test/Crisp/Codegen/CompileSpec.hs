{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Codegen.CompileSpec
-- Description : Tests for LLIR to WebAssembly compilation
--
-- TDD tests for compiling LLIR functions and data types to WebAssembly:
-- - Arithmetic operations
-- - Control flow (if, blocks, loops, branches)
-- - Function calls (direct and indirect)
-- - Data type constructors and pattern matching
-- - Closures
-- - Memory operations

module Crisp.Codegen.CompileSpec (spec) where

import Test.Hspec

import Crisp.Codegen.Compile
import Crisp.Codegen.Wasm (WasmValType(..), WasmInstr(..))
import Crisp.IR.LLIR

import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "LLIR to Wasm Compilation" $ do
    arithmeticTests
    comparisonTests
    localVariableTests
    controlFlowTests
    functionCallTests
    dataTypeTests
    patternMatchTests
    closureTests
    memoryTests
    moduleCompilationTests
    edgeCaseTests

--------------------------------------------------------------------------------
-- Arithmetic Tests
--------------------------------------------------------------------------------

arithmeticTests :: Spec
arithmeticTests = describe "arithmetic operations" $ do
  it "compiles i32 constant" $ do
    let instr = LlirConst (LlirValI32 42)
    compileInstr instr `shouldBe` [WI32Const 42]

  it "compiles i64 constant" $ do
    let instr = LlirConst (LlirValI64 100)
    compileInstr instr `shouldBe` [WI64Const 100]

  it "compiles f64 constant" $ do
    let instr = LlirConst (LlirValF64 3.14)
    compileInstr instr `shouldBe` [WF64Const 3.14]

  it "compiles i32 addition" $ do
    let instr = LlirBinOp LlirAdd
    compileInstr instr `shouldBe` [WI32Add]

  it "compiles i32 subtraction" $ do
    let instr = LlirBinOp LlirSub
    compileInstr instr `shouldBe` [WI32Sub]

  it "compiles i32 multiplication" $ do
    let instr = LlirBinOp LlirMul
    compileInstr instr `shouldBe` [WI32Mul]

  it "compiles nested arithmetic (x + 1)" $ do
    let instrs =
          [ LlirLocalGet 0       -- x
          , LlirConst (LlirValI32 1)
          , LlirBinOp LlirAdd
          ]
    compileInstrs instrs `shouldBe`
      [ WLocalGet 0, WI32Const 1, WI32Add ]

  it "compiles nested arithmetic ((x + y) * 2)" $ do
    let instrs =
          [ LlirLocalGet 0       -- x
          , LlirLocalGet 1       -- y
          , LlirBinOp LlirAdd
          , LlirConst (LlirValI32 2)
          , LlirBinOp LlirMul
          ]
    compileInstrs instrs `shouldBe`
      [ WLocalGet 0, WLocalGet 1, WI32Add, WI32Const 2, WI32Mul ]

  it "compiles division" $ do
    let instr = LlirBinOp LlirDiv
    compileInstr instr `shouldBe` [WI32DivS]

  it "compiles remainder" $ do
    let instr = LlirBinOp LlirRem
    compileInstr instr `shouldBe` [WI32RemS]

  it "compiles bitwise and" $ do
    let instr = LlirBinOp LlirAnd
    compileInstr instr `shouldBe` [WI32And]

  it "compiles bitwise or" $ do
    let instr = LlirBinOp LlirOr
    compileInstr instr `shouldBe` [WI32Or]

  it "compiles bitwise xor" $ do
    let instr = LlirBinOp LlirXor
    compileInstr instr `shouldBe` [WI32Xor]

  it "compiles left shift" $ do
    let instr = LlirBinOp LlirShl
    compileInstr instr `shouldBe` [WI32Shl]

  it "compiles signed right shift" $ do
    let instr = LlirBinOp LlirShr
    compileInstr instr `shouldBe` [WI32ShrS]

  it "compiles unsigned right shift" $ do
    let instr = LlirBinOp LlirShrU
    compileInstr instr `shouldBe` [WI32ShrU]

--------------------------------------------------------------------------------
-- Comparison Tests
--------------------------------------------------------------------------------

comparisonTests :: Spec
comparisonTests = describe "comparison operations" $ do
  it "compiles equality comparison" $ do
    let instr = LlirCmpOp LlirEq
    compileInstr instr `shouldBe` [WI32Eq]

  it "compiles not-equal comparison" $ do
    let instr = LlirCmpOp LlirNe
    compileInstr instr `shouldBe` [WI32Ne]

  it "compiles less-than comparison" $ do
    let instr = LlirCmpOp LlirLt
    compileInstr instr `shouldBe` [WI32LtS]

  it "compiles less-than-or-equal comparison" $ do
    let instr = LlirCmpOp LlirLe
    compileInstr instr `shouldBe` [WI32LeS]

  it "compiles greater-than comparison" $ do
    let instr = LlirCmpOp LlirGt
    compileInstr instr `shouldBe` [WI32GtS]

  it "compiles greater-than-or-equal comparison" $ do
    let instr = LlirCmpOp LlirGe
    compileInstr instr `shouldBe` [WI32GeS]

  it "compiles unsigned less-than comparison" $ do
    let instr = LlirCmpOp LlirLtU
    compileInstr instr `shouldBe` [WI32LtU]

  it "compiles unsigned less-than-or-equal comparison" $ do
    let instr = LlirCmpOp LlirLeU
    compileInstr instr `shouldBe` [WI32LeU]

  it "compiles unsigned greater-than comparison" $ do
    let instr = LlirCmpOp LlirGtU
    compileInstr instr `shouldBe` [WI32GtU]

  it "compiles unsigned greater-than-or-equal comparison" $ do
    let instr = LlirCmpOp LlirGeU
    compileInstr instr `shouldBe` [WI32GeU]

  it "compiles comparison with constants (x == 0)" $ do
    let instrs =
          [ LlirLocalGet 0
          , LlirConst (LlirValI32 0)
          , LlirCmpOp LlirEq
          ]
    compileInstrs instrs `shouldBe`
      [ WLocalGet 0, WI32Const 0, WI32Eq ]

--------------------------------------------------------------------------------
-- Local Variable Tests
--------------------------------------------------------------------------------

localVariableTests :: Spec
localVariableTests = describe "local variable operations" $ do
  it "compiles local.get" $ do
    let instr = LlirLocalGet 0
    compileInstr instr `shouldBe` [WLocalGet 0]

  it "compiles local.get with different index" $ do
    let instr = LlirLocalGet 5
    compileInstr instr `shouldBe` [WLocalGet 5]

  it "compiles local.set" $ do
    let instr = LlirLocalSet 0
    compileInstr instr `shouldBe` [WLocalSet 0]

  it "compiles local.tee" $ do
    let instr = LlirLocalTee 2
    compileInstr instr `shouldBe` [WLocalTee 2]

  it "compiles global.get" $ do
    let instrs = [LlirGlobalGet "heap_ptr"]
    let ctx = defaultCompileContext { ccGlobals = [("heap_ptr", 0)] }
    compileInstrsWithContext ctx instrs `shouldBe` [WGlobalGet 0]

  it "compiles global.set" $ do
    let instrs = [LlirConst (LlirValI32 1000), LlirGlobalSet "heap_ptr"]
    let ctx = defaultCompileContext { ccGlobals = [("heap_ptr", 0)] }
    compileInstrsWithContext ctx instrs `shouldBe` [WI32Const 1000, WGlobalSet 0]

--------------------------------------------------------------------------------
-- Control Flow Tests
--------------------------------------------------------------------------------

controlFlowTests :: Spec
controlFlowTests = describe "control flow" $ do
  it "compiles return" $ do
    let instr = LlirReturn
    compileInstr instr `shouldBe` [WReturn]

  it "compiles unreachable" $ do
    let instr = LlirUnreachable
    compileInstr instr `shouldBe` [WUnreachable]

  it "compiles nop" $ do
    let instr = LlirNop
    compileInstr instr `shouldBe` [WNop]

  it "compiles if-then-else" $ do
    let instr = LlirIf LlirI32
          [LlirConst (LlirValI32 1)]  -- then
          [LlirConst (LlirValI32 0)]  -- else
    compileInstr instr `shouldBe`
      [WIf [WI32Const 1] [WI32Const 0]]

  it "compiles nested if" $ do
    let inner = LlirIf LlirI32
          [LlirConst (LlirValI32 2)]
          [LlirConst (LlirValI32 3)]
    let outer = LlirIf LlirI32
          [inner]
          [LlirConst (LlirValI32 4)]
    compileInstr outer `shouldBe`
      [WIf [WIf [WI32Const 2] [WI32Const 3]] [WI32Const 4]]

  it "compiles block with label" $ do
    let instr = LlirBlock "exit" [LlirConst (LlirValI32 42), LlirReturn]
    -- Block compiles with label resolved to index
    let result = compileInstr instr
    length result `shouldBe` 1
    case head result of
      WBlock _ -> pure ()
      _ -> expectationFailure "Expected WBlock"

  it "compiles loop with label" $ do
    let instr = LlirLoop "loop" [LlirBrIf "loop"]
    let result = compileInstr instr
    length result `shouldBe` 1
    case head result of
      WLoop _ -> pure ()
      _ -> expectationFailure "Expected WLoop"

  it "compiles br (unconditional branch)" $ do
    let instrs = [LlirBlock "exit" [LlirBr "exit"]]
    let result = compileInstrs instrs
    -- The branch should target index 0 (the enclosing block)
    length result `shouldBe` 1

  it "compiles br_if (conditional branch)" $ do
    let instrs = [LlirBlock "check"
          [ LlirLocalGet 0
          , LlirBrIf "check"
          ]]
    let result = compileInstrs instrs
    length result `shouldBe` 1

  it "compiles br_table (switch)" $ do
    let instr = LlirBrTable ["case0", "case1", "case2"] "default"
    -- br_table with label indices
    let result = compileInstr instr
    case head result of
      WBrTable _ _ -> pure ()
      _ -> expectationFailure "Expected WBrTable"

  it "compiles select" $ do
    let instr = LlirSelect
    compileInstr instr `shouldBe` [WSelect]

  it "compiles drop" $ do
    let instr = LlirDrop
    compileInstr instr `shouldBe` [WDrop]

--------------------------------------------------------------------------------
-- Function Call Tests
--------------------------------------------------------------------------------

functionCallTests :: Spec
functionCallTests = describe "function calls" $ do
  it "compiles direct call" $ do
    let instrs =
          [ LlirLocalGet 0
          , LlirLocalGet 1
          , LlirCall "add" 2
          ]
    let ctx = defaultCompileContext { ccFunctions = [("add", 0)] }
    compileInstrsWithContext ctx instrs `shouldBe`
      [ WLocalGet 0, WLocalGet 1, WCall 0 ]

  it "compiles call with no arguments" $ do
    let instrs = [LlirCall "getTime" 0]
    let ctx = defaultCompileContext { ccFunctions = [("getTime", 1)] }
    compileInstrsWithContext ctx instrs `shouldBe` [WCall 1]

  it "compiles multiple calls" $ do
    let instrs =
          [ LlirLocalGet 0
          , LlirCall "f" 1
          , LlirLocalGet 1
          , LlirCall "g" 1
          , LlirBinOp LlirAdd
          ]
    let ctx = defaultCompileContext { ccFunctions = [("f", 0), ("g", 1)] }
    compileInstrsWithContext ctx instrs `shouldBe`
      [ WLocalGet 0, WCall 0, WLocalGet 1, WCall 1, WI32Add ]

  it "compiles indirect call" $ do
    let instr = LlirCallIndirect (LlirFuncType [LlirI32] LlirI32)
    let result = compileInstr instr
    case head result of
      WCallIndirect _ -> pure ()
      _ -> expectationFailure "Expected WCallIndirect"

--------------------------------------------------------------------------------
-- Data Type Tests
--------------------------------------------------------------------------------

dataTypeTests :: Spec
dataTypeTests = describe "data type compilation" $ do
  it "compiles constructor with no payload (None)" $ do
    let instrs = compileConstructor "Option" "None" [] (TagInfo 0 0)
    -- Should allocate 4 bytes (tag only), store tag 0
    length instrs `shouldSatisfy` (> 0)
    -- First instruction should be allocation size
    head instrs `shouldBe` WI32Const 4

  it "compiles constructor with payload (Some)" $ do
    let instrs = compileConstructor "Option" "Some" [LlirI32] (TagInfo 1 4)
    -- Should allocate 8 bytes (tag + i32), store tag 1, store payload
    length instrs `shouldSatisfy` (> 0)
    head instrs `shouldBe` WI32Const 8

  it "compiles pair constructor" $ do
    let instrs = compileConstructor "Pair" "Pair" [LlirI32, LlirI32] (TagInfo 0 8)
    -- Should allocate 12 bytes (tag + two i32s)
    head instrs `shouldBe` WI32Const 12

  it "compiles list cons" $ do
    let instrs = compileConstructor "List" "Cons" [LlirI32, LlirPtr] (TagInfo 1 8)
    -- Should allocate 12 bytes (tag + head + tail pointer)
    head instrs `shouldBe` WI32Const 12

  it "compiles list nil" $ do
    let instrs = compileConstructor "List" "Nil" [] (TagInfo 0 0)
    -- Should allocate 4 bytes (tag only)
    head instrs `shouldBe` WI32Const 4

  it "compiles field access (first of pair)" $ do
    let instrs = compileFieldAccess 0 4  -- First field after tag (offset 4)
    -- Should load at offset 4
    compileInstrs instrs `shouldContain` [WI32Load 4]

  it "compiles field access (second of pair)" $ do
    let instrs = compileFieldAccess 1 4  -- Second field (offset 8)
    -- Should load at offset 8
    compileInstrs instrs `shouldContain` [WI32Load 8]

  it "compiles tag load" $ do
    let instrs = compileTagLoad
    -- Should get pointer from local 0, then load i32 at offset 0
    compileInstrs instrs `shouldBe` [WLocalGet 0, WI32Load 0]

--------------------------------------------------------------------------------
-- Pattern Match Tests
--------------------------------------------------------------------------------

patternMatchTests :: Spec
patternMatchTests = describe "pattern matching compilation" $ do
  it "compiles match on boolean" $ do
    let matchInstrs = compileMatch LlirI32
          [ (0, [LlirConst (LlirValI32 100)])   -- False branch
          , (1, [LlirConst (LlirValI32 200)])   -- True branch
          ]
    -- Should generate if or br_table based on tag
    length matchInstrs `shouldSatisfy` (> 0)

  it "compiles match on Option" $ do
    let matchInstrs = compileMatch LlirI32
          [ (0, [LlirConst (LlirValI32 0)])    -- None: return 0
          , (1, [LlirConst (LlirValI32 1)])    -- Some: return 1
          ]
    length matchInstrs `shouldSatisfy` (> 0)

  it "compiles match with payload extraction" $ do
    let somePayloadInstrs =
          [ LlirLocalGet 0  -- scrutinee pointer
          , LlirLoad LlirI32 4  -- load payload at offset 4
          ]
    compileInstrs somePayloadInstrs `shouldBe`
      [ WLocalGet 0, WI32Load 4 ]

  it "compiles nested match" $ do
    -- Match on Option<Option<Int>>
    -- Some(Some(x)) -> x, Some(None) -> 0, None -> -1
    let innerMatch = compileMatch LlirI32
          [ (0, [LlirConst (LlirValI32 0)])    -- None
          , (1, [LlirLoad LlirI32 4])          -- Some(x) -> load x
          ]
    let outerMatch = compileMatch LlirI32
          [ (0, [LlirConst (LlirValI32 (-1))]) -- None
          , (1, innerMatch)                     -- Some(inner)
          ]
    length outerMatch `shouldSatisfy` (> 0)

  it "compiles match with three or more branches" $ do
    -- Color = Red | Green | Blue
    let matchInstrs = compileMatch LlirI32
          [ (0, [LlirConst (LlirValI32 0xFF0000)])  -- Red
          , (1, [LlirConst (LlirValI32 0x00FF00)])  -- Green
          , (2, [LlirConst (LlirValI32 0x0000FF)])  -- Blue
          ]
    -- Should use br_table for 3+ branches
    length matchInstrs `shouldSatisfy` (> 0)

  it "compiles wildcard pattern" $ do
    -- Default branch catches all unmatched tags
    let matchInstrs = compileMatchWithDefault LlirI32
          [(0, [LlirConst (LlirValI32 1)])]
          [LlirConst (LlirValI32 0)]  -- Default
    length matchInstrs `shouldSatisfy` (> 0)

--------------------------------------------------------------------------------
-- Closure Tests
--------------------------------------------------------------------------------

closureTests :: Spec
closureTests = describe "closure compilation" $ do
  it "compiles closure allocation with no captures" $ do
    let closure = ClosureInfo "f" 0 []
    let instrs = compileClosureAlloc closure
    -- Should allocate 4 bytes (function pointer only)
    head instrs `shouldBe` WI32Const 4

  it "compiles closure allocation with captures" $ do
    let closure = ClosureInfo "f" 0 [("x", LlirI32), ("y", LlirI32)]
    let instrs = compileClosureAlloc closure
    -- Should allocate 12 bytes (func ptr + 2 captures)
    head instrs `shouldBe` WI32Const 12

  it "compiles closure call" $ do
    let closure = ClosureInfo "f" 0 [("x", LlirI32)]
    let instrs = compileClosureCall closure [LlirI32] LlirI32
    -- Should load function pointer and call indirect
    length instrs `shouldSatisfy` (> 0)

  it "compiles closure capture store" $ do
    let instrs = compileCaptureStore 0 4  -- First capture at offset 4
    -- Should store to closure at offset 4
    length (compileInstrs instrs) `shouldSatisfy` (> 0)

  it "compiles closure capture load" $ do
    let instrs = compileCaptureLoad 0 4  -- First capture at offset 4
    -- Should load from closure at offset 4
    compileInstrs instrs `shouldContain` [WI32Load 4]

--------------------------------------------------------------------------------
-- Memory Tests
--------------------------------------------------------------------------------

memoryTests :: Spec
memoryTests = describe "memory operations" $ do
  it "compiles i32 load" $ do
    let instr = LlirLoad LlirI32 0
    compileInstr instr `shouldBe` [WI32Load 0]

  it "compiles i32 load with offset" $ do
    let instr = LlirLoad LlirI32 8
    compileInstr instr `shouldBe` [WI32Load 8]

  it "compiles i64 load" $ do
    let instr = LlirLoad LlirI64 0
    compileInstr instr `shouldBe` [WI64Load 0]

  it "compiles f64 load" $ do
    let instr = LlirLoad LlirF64 0
    compileInstr instr `shouldBe` [WF64Load 0]

  it "compiles i32 store" $ do
    let instr = LlirStore LlirI32 0
    compileInstr instr `shouldBe` [WI32Store 0]

  it "compiles i32 store with offset" $ do
    let instr = LlirStore LlirI32 4
    compileInstr instr `shouldBe` [WI32Store 4]

  it "compiles i64 store" $ do
    let instr = LlirStore LlirI64 0
    compileInstr instr `shouldBe` [WI64Store 0]

  it "compiles f64 store" $ do
    let instr = LlirStore LlirF64 0
    compileInstr instr `shouldBe` [WF64Store 0]

  it "compiles memory.grow" $ do
    let instr = LlirMemoryGrow
    compileInstr instr `shouldBe` [WMemoryGrow]

  it "compiles memory.size" $ do
    let instr = LlirMemorySize
    compileInstr instr `shouldBe` [WMemorySize]

--------------------------------------------------------------------------------
-- Module Compilation Tests
--------------------------------------------------------------------------------

moduleCompilationTests :: Spec
moduleCompilationTests = describe "module compilation" $ do
  it "compiles empty module" $ do
    let llirMod = emptyLlirModule
    let result = compileModule llirMod
    case result of
      Right wasmMod -> length (wasmModuleFunctions wasmMod) `shouldBe` 0
      Left err -> expectationFailure $ "Compilation failed: " ++ show err

  it "compiles module with one function" $ do
    let func = LlirFunc
          { llirFuncName = "answer"
          , llirFuncParams = []
          , llirFuncReturn = LlirI32
          , llirFuncLocals = []
          , llirFuncBody = [LlirConst (LlirValI32 42), LlirReturn]
          }
    let llirMod = emptyLlirModule { llirModFunctions = [func] }
    let result = compileModule llirMod
    case result of
      Right wasmMod -> length (wasmModuleFunctions wasmMod) `shouldBe` 1
      Left err -> expectationFailure $ "Compilation failed: " ++ show err

  it "compiles function with parameters" $ do
    let func = LlirFunc
          { llirFuncName = "add"
          , llirFuncParams = [("x", LlirI32), ("y", LlirI32)]
          , llirFuncReturn = LlirI32
          , llirFuncLocals = []
          , llirFuncBody = [LlirLocalGet 0, LlirLocalGet 1, LlirBinOp LlirAdd, LlirReturn]
          }
    let llirMod = emptyLlirModule { llirModFunctions = [func] }
    let result = compileModule llirMod
    case result of
      Right wasmMod -> do
        length (wasmModuleFunctions wasmMod) `shouldBe` 1
        let wasmFunc = head (wasmModuleFunctions wasmMod)
        wasmFunctionParamCount wasmFunc `shouldBe` 2
      Left err -> expectationFailure $ "Compilation failed: " ++ show err

  it "compiles function with locals" $ do
    let func = LlirFunc
          { llirFuncName = "withLocal"
          , llirFuncParams = [("x", LlirI32)]
          , llirFuncReturn = LlirI32
          , llirFuncLocals = [("temp", LlirI32)]
          , llirFuncBody =
              [ LlirLocalGet 0
              , LlirLocalTee 1
              , LlirLocalGet 1
              , LlirBinOp LlirAdd
              , LlirReturn
              ]
          }
    let llirMod = emptyLlirModule { llirModFunctions = [func] }
    let result = compileModule llirMod
    case result of
      Right wasmMod -> do
        let wasmFunc = head (wasmModuleFunctions wasmMod)
        wasmFunctionLocalCount wasmFunc `shouldBe` 1
      Left err -> expectationFailure $ "Compilation failed: " ++ show err

  it "compiles module with exports" $ do
    let func = LlirFunc
          { llirFuncName = "main"
          , llirFuncParams = []
          , llirFuncReturn = LlirI32
          , llirFuncLocals = []
          , llirFuncBody = [LlirConst (LlirValI32 0), LlirReturn]
          }
    let export = LlirExport "main" (LlirExportFunc "main")
    let llirMod = emptyLlirModule
          { llirModFunctions = [func]
          , llirModExports = [export]
          }
    let result = compileModule llirMod
    case result of
      Right wasmMod -> length (wasmModuleExports wasmMod) `shouldBe` 1
      Left err -> expectationFailure $ "Compilation failed: " ++ show err

  it "compiles module with memory" $ do
    let llirMod = emptyLlirModule
          { llirModMemory = LlirMemory 1 (Just 10)
          }
    let result = compileModule llirMod
    case result of
      Right wasmMod -> wasmModuleHasMemory wasmMod `shouldBe` True
      Left err -> expectationFailure $ "Compilation failed: " ++ show err

  it "compiles module with imports" $ do
    let imp = LlirImport "env" "log" (LlirImportFunc (LlirFuncType [LlirI32] LlirUnit))
    let llirMod = emptyLlirModule { llirModImports = [imp] }
    let result = compileModule llirMod
    case result of
      Right wasmMod -> length (wasmModuleImports wasmMod) `shouldBe` 1
      Left err -> expectationFailure $ "Compilation failed: " ++ show err

--------------------------------------------------------------------------------
-- Edge Case Tests
--------------------------------------------------------------------------------

edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles empty function body" $ do
    let func = LlirFunc
          { llirFuncName = "empty"
          , llirFuncParams = []
          , llirFuncReturn = LlirUnit
          , llirFuncLocals = []
          , llirFuncBody = []
          }
    let llirMod = emptyLlirModule { llirModFunctions = [func] }
    let result = compileModule llirMod
    case result of
      Right _ -> pure ()
      Left err -> expectationFailure $ "Compilation failed: " ++ show err

  it "handles large constant" $ do
    let instr = LlirConst (LlirValI32 2147483647)  -- max i32
    compileInstr instr `shouldBe` [WI32Const 2147483647]

  it "handles negative constant" $ do
    let instr = LlirConst (LlirValI32 (-1))
    compileInstr instr `shouldBe` [WI32Const (-1)]

  it "handles deeply nested blocks" $ do
    let block3 = LlirBlock "b3" [LlirConst (LlirValI32 1)]
    let block2 = LlirBlock "b2" [block3]
    let block1 = LlirBlock "b1" [block2]
    let result = compileInstr block1
    length result `shouldBe` 1

  it "handles many locals" $ do
    let locals = [("x" <> T.pack (show i), LlirI32) | i <- [0..99]]
    let func = LlirFunc
          { llirFuncName = "manyLocals"
          , llirFuncParams = []
          , llirFuncReturn = LlirI32
          , llirFuncLocals = locals
          , llirFuncBody = [LlirLocalGet 50, LlirReturn]
          }
    let llirMod = emptyLlirModule { llirModFunctions = [func] }
    let result = compileModule llirMod
    case result of
      Right wasmMod -> do
        let wasmFunc = head (wasmModuleFunctions wasmMod)
        wasmFunctionLocalCount wasmFunc `shouldBe` 100
      Left err -> expectationFailure $ "Compilation failed: " ++ show err

  it "handles function with many parameters" $ do
    let params = [("p" <> T.pack (show i), LlirI32) | i <- [0..9]]
    let func = LlirFunc
          { llirFuncName = "manyParams"
          , llirFuncParams = params
          , llirFuncReturn = LlirI32
          , llirFuncLocals = []
          , llirFuncBody = [LlirLocalGet 0, LlirReturn]
          }
    let llirMod = emptyLlirModule { llirModFunctions = [func] }
    let result = compileModule llirMod
    case result of
      Right wasmMod -> do
        let wasmFunc = head (wasmModuleFunctions wasmMod)
        wasmFunctionParamCount wasmFunc `shouldBe` 10
      Left err -> expectationFailure $ "Compilation failed: " ++ show err

  it "handles null constant" $ do
    let instr = LlirConst LlirValNull
    compileInstr instr `shouldBe` [WI32Const 0]

  it "handles type conversions" $ do
    let instr = LlirConvert LlirI32 LlirI64
    let result = compileInstr instr
    length result `shouldSatisfy` (>= 1)
