{-# LANGUAGE OverloadedStrings #-}

-- | Compiler Driver
--
-- Core compilation pipeline for quantum-crypto-lang:
--   source -> tokenize -> parse -> analyze -> generate IR -> optimize -> emit
--
-- This module exposes the pipeline as composable functions.

module QCL.Compiler.Driver
  ( CompileResult(..)
  , CompileOptions(..)
  , defaultCompileOptions
  , compile
  , compileToCircuit
  , compileAndVerify
  ) where

import QCL.Syntax.Lexer (tokenize)
import QCL.Syntax.Parser (parseProgram, parseWithErrors)
import QCL.Semantic.Analyzer (analyzeProgram, AnalyzedProgram, prettySemErrors)
import QCL.Semantic.IRGenerator (generateIR, generateIRFromSource)
import QCL.IR.Circuit
import QCL.IR.Wire (verify743WireFixture)
import QCL.Verification.Verifier (verifyCircuit, VerificationResult(..), VerificationLevel(..))

-- | Compilation result
data CompileResult = CompileResult
  { crCircuit :: Maybe Circuit
  , crErrors :: [String]
  , crWarnings :: [String]
  , crSuccess :: Bool
  } deriving (Show, Eq)

-- | Compilation options
data CompileOptions = CompileOptions
  { optOptimize :: Bool
  , optVerify :: Bool
  , optVerbose :: Bool
  , optTarget :: String    -- ^ "openqasm", "quipper", "simulator"
  } deriving (Show, Eq)

-- | Default compilation options
defaultCompileOptions :: CompileOptions
defaultCompileOptions = CompileOptions
  { optOptimize = True
  , optVerify = True
  , optVerbose = False
  , optTarget = "openqasm"
  }

-- | Compile source code to circuit
compile :: CompileOptions -> String -> CompileResult
compile opts source =
  case generateIRFromSource source of
    Left errs ->
      CompileResult Nothing (map show errs) [] False
    Right circ ->
      if optVerify opts
        then let vr = verifyCircuit StandardVerify circ
             in if vrPassed vr
                then CompileResult (Just circ) [] (vrWarnings vr) True
                else CompileResult (Just circ) (vrErrors vr) (vrWarnings vr) False
        else CompileResult (Just circ) [] [] True

-- | Compile to circuit without verification
compileToCircuit :: String -> Either String Circuit
compileToCircuit source =
  case generateIRFromSource source of
    Left errs -> Left (show errs)
    Right circ -> Right circ

-- | Compile and verify at full level
compileAndVerify :: String -> CompileResult
compileAndVerify source =
  case generateIRFromSource source of
    Left errs ->
      CompileResult Nothing (map show errs) [] False
    Right circ ->
      let vr = verifyCircuit FullVerify circ
      in CompileResult (Just circ) (vrErrors vr) (vrWarnings vr) (vrPassed vr)
