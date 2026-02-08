{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec

import qualified Crisp.Lexer.LexerSpec
import qualified Crisp.Lexer.ErrorSpec
import qualified Crisp.Parser.ParserSpec
import qualified Crisp.Types.CheckerSpec
import qualified Crisp.Types.UnifySpec
import qualified Crisp.Types.InferSpec
import qualified Crisp.Types.KindSpec
import qualified Crisp.Types.ExhaustiveSpec
import qualified Crisp.Effects.TypingSpec
import qualified Crisp.Effects.PolymorphismSpec
import qualified Crisp.Effects.HandlerSpec
import qualified Crisp.Types.ConstructorSpec
import qualified Crisp.Core.PatternSpec
import qualified Crisp.Core.DesugarSpec
import qualified Crisp.IR.CPSSpec
import qualified Crisp.IR.LLIRSpec
import qualified Crisp.IR.ENIRSpec
import qualified Crisp.Types.DependentSpec
import qualified Crisp.Types.GADTSpec
import qualified Crisp.Types.PropSpec
import qualified Crisp.Codegen.WasmBinarySpec
import qualified Crisp.Codegen.CompileSpec
import qualified Crisp.Codegen.WasmSpec
import qualified Crisp.Manifest.ManifestSpec
import qualified Crisp.Module.ModuleSpec
import qualified Crisp.Types.LinearSpec
import qualified Crisp.Types.TraitSpec
import qualified Crisp.FFI.ExternalSpec
import qualified Crisp.FFI.TimeSpec
import qualified Crisp.FFI.HttpSpec
import qualified Crisp.REPL.ReplSpec
import qualified Crisp.Types.RefinementSpec
import qualified Crisp.Types.BoundedPolySpec
import qualified Crisp.Types.TypeAliasSpec
import qualified Crisp.Types.DeriveSpec
import qualified Crisp.Runtime.AllocatorSpec
import qualified Crisp.Formatter.FormatSpec
import qualified Crisp.Prelude.PreludeSpec
import qualified Crisp.Prelude.NumericSpec
import qualified Crisp.LSP.ServerSpec
import qualified Crisp.Doc.GenerateSpec

main :: IO ()
main = hspec $ do
  describe "Crisp.Lexer" Crisp.Lexer.LexerSpec.spec
  describe "Crisp.Lexer.Error" Crisp.Lexer.ErrorSpec.spec
  describe "Crisp.Parser" Crisp.Parser.ParserSpec.spec
  describe "Crisp.Types.Checker" Crisp.Types.CheckerSpec.spec
  describe "Crisp.Types.Unify" Crisp.Types.UnifySpec.spec
  describe "Crisp.Types.Infer" Crisp.Types.InferSpec.spec
  describe "Crisp.Types.Kind" Crisp.Types.KindSpec.spec
  describe "Crisp.Types.Exhaustive" Crisp.Types.ExhaustiveSpec.spec
  describe "Crisp.Effects.Typing" Crisp.Effects.TypingSpec.spec
  describe "Crisp.Effects.Polymorphism" Crisp.Effects.PolymorphismSpec.spec
  describe "Crisp.Effects.Handler" Crisp.Effects.HandlerSpec.spec
  describe "Crisp.Types.Constructor" Crisp.Types.ConstructorSpec.spec
  describe "Crisp.Core.Pattern" Crisp.Core.PatternSpec.spec
  describe "Crisp.Core.Desugar" Crisp.Core.DesugarSpec.spec
  describe "Crisp.IR.CPS" Crisp.IR.CPSSpec.spec
  describe "Crisp.IR.LLIR" Crisp.IR.LLIRSpec.spec
  describe "Crisp.IR.ENIR" Crisp.IR.ENIRSpec.spec
  describe "Crisp.Types.Dependent" Crisp.Types.DependentSpec.spec
  describe "Crisp.Types.GADT" Crisp.Types.GADTSpec.spec
  describe "Crisp.Types.Prop" Crisp.Types.PropSpec.spec
  describe "Crisp.Codegen.WasmBinary" Crisp.Codegen.WasmBinarySpec.spec
  describe "Crisp.Codegen.Compile" Crisp.Codegen.CompileSpec.spec
  describe "Crisp.Codegen.Wasm" Crisp.Codegen.WasmSpec.spec
  describe "Crisp.Manifest" Crisp.Manifest.ManifestSpec.spec
  describe "Crisp.Module" Crisp.Module.ModuleSpec.spec
  describe "Crisp.Types.Linear" Crisp.Types.LinearSpec.spec
  describe "Crisp.Types.Trait" Crisp.Types.TraitSpec.spec
  describe "Crisp.FFI.External" Crisp.FFI.ExternalSpec.spec
  describe "Crisp.FFI.Time" Crisp.FFI.TimeSpec.spec
  describe "Crisp.FFI.Http" Crisp.FFI.HttpSpec.spec
  describe "Crisp.REPL" Crisp.REPL.ReplSpec.spec
  describe "Crisp.Types.Refinement" Crisp.Types.RefinementSpec.spec
  describe "Crisp.Types.BoundedPoly" Crisp.Types.BoundedPolySpec.spec
  describe "Crisp.Types.TypeAlias" Crisp.Types.TypeAliasSpec.spec
  describe "Crisp.Types.Derive" Crisp.Types.DeriveSpec.spec
  describe "Crisp.Runtime.Allocator" Crisp.Runtime.AllocatorSpec.spec
  describe "Crisp.Formatter" Crisp.Formatter.FormatSpec.spec
  describe "Crisp.Prelude" Crisp.Prelude.PreludeSpec.spec
  describe "Crisp.Prelude.Numeric" Crisp.Prelude.NumericSpec.spec
  describe "Crisp.LSP.Server" Crisp.LSP.ServerSpec.spec
  describe "Crisp.Doc.Generate" Crisp.Doc.GenerateSpec.spec
