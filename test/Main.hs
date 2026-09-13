{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for quantum-crypto-lang
-- Tests core functionality of the QCL compiler

module Main where

import System.Exit (exitFailure, exitSuccess)
import qualified Tests.Wire as Wire
import qualified Tests.Gate as Gate
import qualified Tests.Parser as Parser
import qualified Tests.Fixture as Fixture

main :: IO ()
main = do
  putStrLn "Running quantum-crypto-lang test suite..."

  -- Run test groups
  let results =
        [ ("Wire Tests", Wire.runTests)
        , ("Gate Tests", Gate.runTests)
        , ("Parser Tests", Parser.runTests)
        , ("Fixture Tests", Fixture.runTests)
        ]

  passed <- sum <$> mapM runTestGroup results
  let total = length results

  putStrLn $ "\n" ++ show passed ++ "/" ++ show total ++ " test groups passed"
  if passed == total then exitSuccess else exitFailure

runTestGroup :: (String, IO Bool) -> IO Int
runTestGroup (name, action) = do
  putStrLn $ "\n[" ++ name ++ "]"
  result <- action
  if result then return 1 else return 0
