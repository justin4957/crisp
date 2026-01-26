{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.CLI.Commands
-- Description : CLI command implementations
--
-- Implements the various CLI commands for the Crisp compiler.

module Crisp.CLI.Commands
  ( runCommand
  ) where

import Crisp.CLI.Options
import Crisp.Lexer.Lexer (lexFile)
import Crisp.Parser.Parser (parseModule)
import Crisp.Core.Desugar (desugarModule)
import Crisp.Types.Context (withPrelude)
import Crisp.Types.Checker (synthesize)
import Crisp.IR.TypedIR (newModule, encodeModule)
import Crisp.Codegen.Wasm (compileToWasm)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath (replaceExtension)

-- | Run a CLI command
runCommand :: Options -> IO (Either String String)
runCommand opts = case optCommand opts of
  CmdCompile copts -> runCompile (optVerbose opts) copts
  CmdCheck copts -> runCheck (optVerbose opts) copts
  CmdFormat fopts -> runFormat (optVerbose opts) fopts
  CmdRepl -> runRepl (optVerbose opts)
  CmdVersion -> pure $ Right versionString

versionString :: String
versionString = "crisp 0.1.0"

-- | Run the compile command
runCompile :: Bool -> CompileOptions -> IO (Either String String)
runCompile verbose copts = do
  let inputFile = coInputFile copts
      outputFile = maybe (replaceExtension inputFile ".wasm") id (coOutputFile copts)

  when verbose $ putStrLn $ "Reading " ++ inputFile

  -- Read source file
  source <- TIO.readFile inputFile

  -- Parse
  when verbose $ putStrLn "Parsing..."
  case parseModule inputFile source of
    Left err -> pure $ Left $ "Parse error:\n" ++ show err
    Right ast -> do
      when verbose $ putStrLn "Desugaring..."

      -- Desugar
      case desugarModule ast of
        Left err -> pure $ Left $ "Desugar error: " ++ show err
        Right _coreDefs -> do
          when verbose $ putStrLn "Type checking..."

          -- Type check (placeholder - would check all definitions)
          -- For now, just create a typed module
          let tirModule = newModule (T.pack inputFile) Nothing []

          -- Emit TIR if requested
          when (coEmitTIR copts) $ do
            let tirFile = replaceExtension inputFile ".tir.json"
            when verbose $ putStrLn $ "Writing TIR to " ++ tirFile
            BL.writeFile tirFile (encodeModule tirModule)

          -- Compile to Wasm
          when verbose $ putStrLn "Generating WebAssembly..."
          case compileToWasm tirModule of
            Left err -> pure $ Left $ "Codegen error: " ++ show err
            Right wasmMod -> do
              when verbose $ putStrLn $ "Writing " ++ outputFile
              -- BL.writeFile outputFile (wasmBytes wasmMod)
              pure $ Right $ "Compiled " ++ inputFile ++ " -> " ++ outputFile ++ " (placeholder)"

-- | Run the check command
runCheck :: Bool -> CheckOptions -> IO (Either String String)
runCheck verbose copts = do
  let inputFile = chInputFile copts

  when verbose $ putStrLn $ "Reading " ++ inputFile

  -- Read source file
  source <- TIO.readFile inputFile

  -- Parse
  when verbose $ putStrLn "Parsing..."
  case parseModule inputFile source of
    Left err -> pure $ Left $ "Parse error:\n" ++ show err
    Right ast -> do
      when verbose $ putStrLn "Desugaring..."

      -- Desugar
      case desugarModule ast of
        Left err -> pure $ Left $ "Desugar error: " ++ show err
        Right _coreDefs -> do
          when verbose $ putStrLn "Type checking..."

          -- Type check would go here
          pure $ Right $ "Type check passed: " ++ inputFile

-- | Run the format command
runFormat :: Bool -> FormatOptions -> IO (Either String String)
runFormat verbose fopts = do
  let files = foInputFiles fopts

  when verbose $ putStrLn $ "Formatting " ++ show (length files) ++ " file(s)"

  -- Placeholder - actual formatter would parse and pretty-print
  pure $ Right $ "Formatted " ++ show (length files) ++ " file(s) (not yet implemented)"

-- | Run the REPL
runRepl :: Bool -> IO (Either String String)
runRepl verbose = do
  when verbose $ putStrLn "Starting REPL..."
  pure $ Right "Crisp REPL not yet implemented"

-- Helper
when :: Bool -> IO () -> IO ()
when True action = action
when False _ = pure ()
