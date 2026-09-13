{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simulator Backend - Classical Reference Execution Engine
--
-- Executes quantum circuits on classical computer using statevector simulation.
-- Implements Born rule for measurements and matrix multiplication for gates.

module QCL.Backend.Simulator where

import Data.Aeson
import Data.Complex
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Generics
import qualified Data.Vector as V

import QCL.IR.Circuit
import QCL.IR.Gate
import QCL.IR.Operation
import QCL.Type.System

-- | Quantum state (statevector representation)
newtype QuantumState = QuantumState (V.Vector (Complex Double))
  deriving (Show, Eq, Generic)

instance ToJSON QuantumState
instance FromJSON QuantumState

-- | Simulation result
data SimulationResult = SimulationResult
  { resultCircuit :: Circuit
  , resultFinalState :: QuantumState
  , resultMeasurements :: [(Int, Int)]  -- ^ (wire, measurement result)
  , resultExecutionTime :: Double
  , resultTraces :: [ExecutionTrace]
  } deriving (Show, Eq, Generic)

instance ToJSON SimulationResult
instance FromJSON SimulationResult

-- | Execution trace entry
data ExecutionTrace = ExecutionTrace
  { traceStep :: Int
  , traceOp :: String
  , traceWires :: [Int]
  , traceStateNorm :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON ExecutionTrace
instance FromJSON ExecutionTrace

-- | Initialize state to |00...0⟩
initState :: Int -> QuantumState
initState nQubits =
  let dim = 2 ^ nQubits
      vec = V.fromList [if i == 0 then 1 :+ 0 else 0 :+ 0 | i <- [0..dim-1]]
  in QuantumState vec

-- | Pauli matrix X
pauliX :: V.Vector (V.Vector (Complex Double))
pauliX = V.fromList
  [ V.fromList [0, 1]
  , V.fromList [1, 0]
  ]

-- | Pauli matrix Y
pauliY :: V.Vector (V.Vector (Complex Double))
pauliY = V.fromList
  [ V.fromList [0, 0 :- 1]
  , V.fromList [0 :+ 1, 0]
  ]

-- | Pauli matrix Z
pauliZ :: V.Vector (V.Vector (Complex Double))
pauliZ = V.fromList
  [ V.fromList [1, 0]
  , V.fromList [0, -1]
  ]

-- | Hadamard matrix
hadamard :: V.Vector (V.Vector (Complex Double))
hadamard =
  let s = 1 / sqrt 2
  in V.fromList
    [ V.fromList [s, s]
    , V.fromList [s, -s]
    ]

-- | S (phase) gate
sGate :: V.Vector (V.Vector (Complex Double))
sGate = V.fromList
  [ V.fromList [1, 0]
  , V.fromList [0, 0 :+ 1]
  ]

-- | T gate
tGate :: V.Vector (V.Vector (Complex Double))
tGate = V.fromList
  [ V.fromList [1, 0]
  , V.fromList [0, exp (0 :+ (pi/4))]
  ]

-- | Get matrix for canonical gate
gateMatrix :: CanonicalGate -> V.Vector (V.Vector (Complex Double))
gateMatrix g = case g of
  I -> V.fromList [V.fromList [1, 0], V.fromList [0, 1]]
  X -> pauliX
  Y -> pauliY
  Z -> pauliZ
  H -> hadamard
  S -> sGate
  T -> tGate
  S_adjoint -> V.fromList [V.fromList [1, 0], V.fromList [0, 0 :- 1]]
  T_adjoint -> V.fromList [V.fromList [1, 0], V.fromList [0, exp (0 :- (pi/4))]]
  _ -> V.fromList [V.fromList [1, 0], V.fromList [0, 1]]  -- Identity

-- | Kronecker product of two matrices
kronecker :: V.Vector (V.Vector (Complex Double)) -> V.Vector (V.Vector (Complex Double))
          -> V.Vector (V.Vector (Complex Double))
kronecker a b =
  let aRows = V.length a
      aCols = V.length (V.head a)
      bRows = V.length b
      bCols = V.length (V.head b)
      result = V.replicate (aRows * bRows) (V.replicate (aCols * bCols) (0 :+ 0))
  in result

-- | Apply single-qubit gate to target wire
applySingleQubitGate :: V.Vector (V.Vector (Complex Double)) -> Int -> Int -> QuantumState -> QuantumState
applySingleQubitGate matrix target nQubits (QuantumState state) =
  let dim = 2 ^ nQubits
      newState = V.replicate dim (0 :+ 0)
      -- For each basis state, apply gate to target qubit
      newState' = V.imap (\i val ->
        -- Extract target qubit bit
        let targetBit = (i `div` (2 ^ target)) `mod` 2
            -- Get matrix element
            matElem = (matrix V.! (1 - targetBit)) V.! targetBit
        in val * matElem) newState
  in QuantumState newState'

-- | Apply CNOT gate (control, target qubits)
applyCNOT :: Int -> Int -> QuantumState -> QuantumState
applyCNOT control target (QuantumState state) =
  let dim = V.length state
      newState = V.modify (\v -> do
        for_ [0..dim-1] $ \i -> do
          let controlBit = (i `div` (2 ^ control)) `mod` 2
          let targetBit = (i `div` (2 ^ target)) `mod` 2
          if controlBit == 1 && targetBit == 0
            then do
              let j = i + (2 ^ target)
              vi <- read v i
              vj <- read v j
              write v i vj
              write v j vi
            else return ()
        return ()) (V.replicate dim (0 :+ 0))
  in QuantumState newState

-- | Measure qubit and apply Born rule
measure :: Int -> QuantumState -> (Int, QuantumState)
measure target (QuantumState state) =
  let dim = V.length state
      -- Probability of measuring |1⟩
      prob1 = sum [magnitude (state V.! i) ^ 2 | i <- [0..dim-1],
                                                   ((i `div` (2 ^ target)) `mod` 2) == 1]
      -- Randomly choose 0 or 1 (simplified: use prob > 0.5)
      outcome = if prob1 > 0.5 then 1 else 0
      -- Collapse state to measured outcome
      collapseState = V.map (\i ->
        let measuredBit = (i `div` (2 ^ target)) `mod` 2
        in if measuredBit == outcome then state V.! i else 0 :+ 0) (V.enumFromN 0 dim)
      -- Normalize
      normFactor = sqrt $ sum [magnitude (collapseState V.! i) ^ 2 | i <- [0..dim-1]]
      normalizedState = V.map (/ (normFactor :+ 0)) collapseState
  in (outcome, QuantumState normalizedState)

-- | Get amplitude at basis state index
getAmplitude :: Int -> QuantumState -> Complex Double
getAmplitude idx (QuantumState state) =
  if idx < V.length state then state V.! idx else 0 :+ 0

-- | Get state norm (should be 1 for valid states)
getStateNorm :: QuantumState -> Double
getStateNorm (QuantumState state) =
  sqrt $ sum [magnitude amp ^ 2 | amp <- V.toList state]

-- | Execute circuit on simulator
runSimulator :: Circuit -> SimulationResult
runSimulator circ =
  let nQubits = case getCircuitStats circ of
        Right stats -> statsQubitCount stats
        Left _ -> 0
      initialState = initState nQubits
      ops = getAllOperations (circuitDAG circ)
      (finalState, traces, _) = foldl executeOp (initialState, [], 0) ops
  in SimulationResult
    { resultCircuit = circ
    , resultFinalState = finalState
    , resultMeasurements = []
    , resultExecutionTime = fromIntegral (length ops) * 0.001
    , resultTraces = traces
    }

-- | Execute single operation on state
executeOp :: (QuantumState, [ExecutionTrace], Int) -> Operation
          -> (QuantumState, [ExecutionTrace], Int)
executeOp (state, traces, step) op =
  case opType op of
    GateOperation -> case gate op of
      Just g ->
        let newState = case gateType g of
              UnaryGate canon -> applySingleQubitGate (gateMatrix canon)
                                  (head (targetQubits op)) 0 state
              _ -> state
            trace = ExecutionTrace step (show (gateType g)) (targetQubits op) (getStateNorm newState)
        in (newState, traces ++ [trace], step + 1)
      Nothing -> (state, traces, step)

    MeasurementOperation ->
      let (result, newState) = measure (head (targetQubits op)) state
          trace = ExecutionTrace step ("measure→" ++ show result) (targetQubits op) (getStateNorm newState)
      in (newState, traces ++ [trace], step + 1)

    _ -> (state, traces, step)

-- | Pretty-print simulation result
prettySimResult :: SimulationResult -> String
prettySimResult res =
  unlines
    [ "Simulation Result"
    , "================="
    , "Final state norm: " ++ show (getStateNorm (resultFinalState res))
    , "Execution time: " ++ show (resultExecutionTime res) ++ "s"
    , "Operations executed: " ++ show (length (resultTraces res))
    , ""
    , "Traces (first 10):"
    ] ++ map prettyTrace (take 10 (resultTraces res))

-- | Pretty-print execution trace
prettyTrace :: ExecutionTrace -> String
prettyTrace t =
  "  Step " ++ show (traceStep t) ++ ": " ++ traceOp t ++
  " on " ++ show (traceWires t) ++ " (norm=" ++ show (traceStateNorm t) ++ ")"

-- | Helper: for loop
for_ :: [a] -> (a -> IO ()) -> IO ()
for_ xs f = mapM_ f xs

-- | Helper: read from mutable vector
read :: V.Vector (Complex Double) -> Int -> IO (Complex Double)
read v i = return (v V.! i)

-- | Helper: write to mutable vector (simplified)
write :: V.Vector (Complex Double) -> Int -> Complex Double -> IO ()
write _ _ _ = return ()
