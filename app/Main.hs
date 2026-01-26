{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Crisp Compiler CLI
--
-- Main entry point for the Crisp compiler.

module Main (main) where

import Crisp.CLI.Commands (runCommand)
import Crisp.CLI.Options (parseOptions, Options(..))

import System.Exit (exitWith, ExitCode(..))

main :: IO ()
main = do
  opts <- parseOptions
  result <- runCommand opts
  case result of
    Right output -> putStrLn output
    Left err -> do
      putStrLn $ "Error: " ++ err
      exitWith (ExitFailure 1)
