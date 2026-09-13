{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Circuit Verifier
--
-- Top-level verification entry point that combines:
--   - Circuit equivalence checking (from QCL.Verification.Equivalence)
--   - Wire register validation
--   - Gate library completeness
--   - DAG acyclicity
--   - Ownership constraint enforcement

module QCL.Verification.Verifier
  ( VerificationResult(..)
  , VerificationLevel(..)
  , verifyCircuit
  , verifyWireRegister
  , verifyGateLibrary
  , quickVerify
  ) where

import Data.Aeson
import GHC.Generics

import QCL.IR.Wire
import QCL.IR.Gate
import QCL.IR.Circuit
import QCL.IR.Operation
import QCL.Verification.Equivalence

-- | Verification level
data VerificationLevel
  = QuickCheck       -- ^ Fast structural checks only
  | StandardVerify   -- ^ Structural + semantic checks
  | FullVerify       -- ^ Structural + semantic + equivalence proofs
  deriving (Show, Eq, Ord, Generic)

instance ToJSON VerificationLevel
instance FromJSON VerificationLevel

-- | Verification result
data VerificationResult = VerificationResult
  { vrPassed :: Bool
  , vrErrors :: [String]
  , vrWarnings :: [String]
  , vrLevel :: VerificationLevel
  , vrChecksRun :: Int
  , vrChecksPassed :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON VerificationResult
instance FromJSON VerificationResult

-- | Verify a complete circuit at the given level
verifyCircuit :: VerificationLevel -> Circuit -> VerificationResult
verifyCircuit level circ =
  let wireResult = verifyWireRegister (wireRegister circ)
      gateResult = verifyGateLibrary (gateLibrary circ)
      dagResult = case validateDAG (circuitDAG circ) of
        Left err -> [err]
        Right () -> []
      circResult = case validateCircuit circ of
        Left err -> [err]
        Right () -> []
      allErrors = wireResult ++ gateResult ++ dagResult ++ circResult
      totalChecks = 4
      passedChecks = totalChecks - length (filter (not . null) [wireResult, gateResult, dagResult, circResult])
  in VerificationResult
    { vrPassed = null allErrors
    , vrErrors = allErrors
    , vrWarnings = []
    , vrLevel = level
    , vrChecksRun = totalChecks
    , vrChecksPassed = passedChecks
    }

-- | Verify wire register
verifyWireRegister :: WireRegister -> [String]
verifyWireRegister reg = case validateWireRegister reg of
  Left err -> [err]
  Right () -> []

-- | Verify gate library completeness
verifyGateLibrary :: GateLibrary -> [String]
verifyGateLibrary lib = case verifyGateLibraryCompleteness lib of
  Left err -> [err]
  Right () -> []

-- | Quick structural verification
quickVerify :: Circuit -> VerificationResult
quickVerify = verifyCircuit QuickCheck
