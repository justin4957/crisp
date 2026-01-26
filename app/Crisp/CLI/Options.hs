{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : Crisp.CLI.Options
-- Description : Command-line option parsing
--
-- Parses command-line arguments for the Crisp compiler.

module Crisp.CLI.Options
  ( Options(..)
  , Command(..)
  , CompileOptions(..)
  , CheckOptions(..)
  , FormatOptions(..)
  , parseOptions
  ) where

import Options.Applicative

import Data.Text (Text)
import qualified Data.Text as T

-- | Top-level options
data Options = Options
  { optVerbose :: !Bool
  , optCommand :: !Command
  } deriving stock (Eq, Show)

-- | Available commands
data Command
  = CmdCompile !CompileOptions
  | CmdCheck !CheckOptions
  | CmdFormat !FormatOptions
  | CmdRepl
  | CmdVersion
  deriving stock (Eq, Show)

-- | Options for the compile command
data CompileOptions = CompileOptions
  { coInputFile  :: !FilePath
  , coOutputFile :: !(Maybe FilePath)
  , coEmitTIR    :: !Bool
  } deriving stock (Eq, Show)

-- | Options for the check command
data CheckOptions = CheckOptions
  { chInputFile :: !FilePath
  } deriving stock (Eq, Show)

-- | Options for the format command
data FormatOptions = FormatOptions
  { foInputFiles :: ![FilePath]
  , foInPlace    :: !Bool
  } deriving stock (Eq, Show)

-- | Parse command-line options
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Crisp - A language with dependent types and algebraic effects"
     <> header "crisp - Compile Crisp programs to WebAssembly"
      )

optionsParser :: Parser Options
optionsParser = Options
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose output"
      )
  <*> commandParser

commandParser :: Parser Command
commandParser = subparser
  ( command "compile" (info compileParser (progDesc "Compile a Crisp source file"))
 <> command "check" (info checkParser (progDesc "Type-check a Crisp source file"))
 <> command "format" (info formatParser (progDesc "Format Crisp source files"))
 <> command "repl" (info (pure CmdRepl) (progDesc "Start an interactive REPL"))
 <> command "version" (info (pure CmdVersion) (progDesc "Print version information"))
  )

compileParser :: Parser Command
compileParser = CmdCompile <$> (CompileOptions
  <$> argument str
      ( metavar "FILE"
     <> help "Input Crisp source file"
      )
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Output file path"
      ))
  <*> switch
      ( long "emit-tir"
     <> help "Emit Typed IR as JSON artifact"
      ))

checkParser :: Parser Command
checkParser = CmdCheck <$> (CheckOptions
  <$> argument str
      ( metavar "FILE"
     <> help "Input Crisp source file"
      ))

formatParser :: Parser Command
formatParser = CmdFormat <$> (FormatOptions
  <$> some (argument str
      ( metavar "FILES..."
     <> help "Input Crisp source files"
      ))
  <*> switch
      ( long "in-place"
     <> short 'i'
     <> help "Format files in place"
      ))
