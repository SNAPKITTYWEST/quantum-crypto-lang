{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for quantum-crypto-lang
-- Uses tasty framework for structured test execution.

module Main where

import Test.Tasty

import qualified Tests.Wire as Wire
import qualified Tests.Gate as Gate
import qualified Tests.Parser as Parser
import qualified Tests.Fixture as Fixture
import qualified Tests.OpenQASM as OpenQASM
import qualified Tests.Quipper as Quipper

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "quantum-crypto-lang"
  [ Wire.tests
  , Gate.tests
  , Parser.tests
  , Fixture.tests
  , OpenQASM.tests
  , Quipper.tests
  ]
