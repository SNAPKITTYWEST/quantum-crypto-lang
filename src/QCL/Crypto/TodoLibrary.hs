{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module QCL.Crypto.TodoLibrary where

import Data.Aeson
import Data.Time
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Monad.State
import Data.List (sortBy)
import Data.Ord (comparing)

-- | Todo status in the quantum-crypto-lang build lifecycle
data TodoStatus
  = InProgress
  | Pending
  | Completed
  | Failed
  | Blocked
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TodoStatus
instance FromJSON TodoStatus

-- | Priority level for todos
data Priority = Critical | High | Medium | Low
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Priority
instance FromJSON Priority

-- | A single todo in the crypto build workflow
data Todo = Todo
  { todoId :: Int
  , content :: String
  , status :: TodoStatus
  , priority :: Priority
  , dependencies :: [Int]  -- ^ IDs of todos that must complete first
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , completedAt :: Maybe UTCTime
  , moduleName :: String
  , toolId :: String
  , executionTime :: Maybe Double  -- ^ seconds
  , error :: Maybe String
  } deriving (Show, Eq, Generic)

instance ToJSON Todo
instance FromJSON Todo

-- | Execution request sent to async daemon
data ExecutionRequest = ExecutionRequest
  { requestId :: String
  , toolId :: String
  , operation :: String
  , arguments :: Map.Map String String
  , priority :: Int
  , deadline :: Maybe UTCTime
  , authorizationContext :: AuthContext
  , correlationId :: String
  } deriving (Show, Eq, Generic)

instance ToJSON ExecutionRequest
instance FromJSON ExecutionRequest

-- | Response from async daemon
data ExecutionResponse = ExecutionResponse
  { requestId :: String
  , status :: ExecutionStatus
  , result :: Maybe String
  , error :: Maybe String
  , telemetry :: Telemetry
  , startedAt :: UTCTime
  , completedAt :: UTCTime
  , backend :: String
  } deriving (Show, Eq, Generic)

instance ToJSON ExecutionResponse
instance FromJSON ExecutionResponse

-- | Execution status returned by daemon
data ExecutionStatus
  = Received
  | Validating
  | Authorized
  | Queued
  | Running
  | Observing
  | ExecutionCompleted
  | Rejected
  | Cancelled
  | ExecutionTimeout
  | ExecutionFailed
  | BackendError
  | PolicyBlocked
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ExecutionStatus
instance FromJSON ExecutionStatus

-- | Authorization context for policy enforcement
data AuthContext = AuthContext
  { userId :: String
  , permissions :: [String]
  , scope :: String
  , trustLevel :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON AuthContext
instance FromJSON AuthContext

-- | Telemetry collected during execution
data Telemetry = Telemetry
  { queueWaitTime :: Double
  , executionTime :: Double
  , resourceUsage :: ResourceUsage
  , events :: [TelemetryEvent]
  } deriving (Show, Eq, Generic)

instance ToJSON Telemetry
instance FromJSON Telemetry

-- | Resource usage during execution
data ResourceUsage = ResourceUsage
  { cpuPercent :: Double
  , memoryMB :: Double
  , diskIOOps :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON ResourceUsage
instance FromJSON ResourceUsage

-- | Individual telemetry event
data TelemetryEvent = TelemetryEvent
  { eventType :: String
  , timestamp :: UTCTime
  , message :: String
  } deriving (Show, Eq, Generic)

instance ToJSON TelemetryEvent
instance FromJSON TelemetryEvent

-- | Todo database state
data TodoState = TodoState
  { todos :: Map.Map Int Todo
  , nextId :: Int
  , lastUpdated :: UTCTime
  , executionLog :: [ExecutionLog]
  } deriving (Show, Eq)

-- | Execution audit log entry
data ExecutionLog = ExecutionLog
  { logId :: String
  , logTodoId :: Int
  , requestSent :: ExecutionRequest
  , responseReceived :: ExecutionResponse
  , policyDecision :: PolicyDecision
  , finalOutcome :: String
  , logTimestamp :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON ExecutionLog
instance FromJSON ExecutionLog

-- | Policy decision from daemon
data PolicyDecision
  = Allowed
  | DeniedSafety String
  | DeniedAuth String
  | DeniedResource String
  deriving (Show, Eq, Generic)

instance ToJSON PolicyDecision
instance FromJSON PolicyDecision

-- | Dependency resolution result
data DependencyResolution = DependencyResolution
  { canExecute :: Bool
  , blockedBy :: [Int]
  , readyToExecute :: [Int]
  } deriving (Show, Eq)

-- | Async daemon state
data DaemonState
  = DaemonReceived
  | DaemonValidating
  | DaemonAuthorized
  | DaemonQueued
  | DaemonRunning
  | DaemonObserving
  | DaemonCompleted
  deriving (Show, Eq, Ord, Generic)

instance ToJSON DaemonState
instance FromJSON DaemonState

-- | The 18 quantum-crypto-lang todos
quantumCryptoLangTodos :: [Todo]
quantumCryptoLangTodos =
  [ Todo 1 "Create project structure and cabal file for quantum-crypto-lang" InProgress High []
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "ProjectStructure" "qcl.init" Nothing Nothing

  , Todo 2 "Build Wire model with 743-wire extraction and canonical mapping" Pending High [1]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.IR.Wire" "qcl.build.wire" Nothing Nothing

  , Todo 3 "Build Gate model with canonical gate vocabulary" Pending High [2]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.IR.Gate" "qcl.build.gate" Nothing Nothing

  , Todo 4 "Build Operation and Circuit IR (directed dependency graph)" Pending High [3]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.IR.Operation" "qcl.build.operation" Nothing Nothing

  , Todo 5 "Build Quantum Register model" Pending High [4]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.IR.Register" "qcl.build.register" Nothing Nothing

  , Todo 6 "Build Type System with quantum types" Pending High [5]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Type.System" "qcl.build.typesystem" Nothing Nothing

  , Todo 7 "Build Linear/Affine Ownership Checker" Pending High [6]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Type.Ownership" "qcl.build.ownership" Nothing Nothing

  , Todo 8 "Build Lexer for quantum cryptographic language" Pending High [7]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Syntax.Lexer" "qcl.build.lexer" Nothing Nothing

  , Todo 9 "Build Parser and AST" Pending High [8]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Syntax.Parser" "qcl.build.parser" Nothing Nothing

  , Todo 10 "Build Semantic Analyzer and IR Generator" Pending High [9]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Semantic.Analyzer" "qcl.build.semantic" Nothing Nothing

  , Todo 11 "Build Circuit Optimizer" Pending High [10]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Optimization.Optimizer" "qcl.build.optimizer" Nothing Nothing

  , Todo 12 "Build Circuit Equivalence and Verifier" Pending High [11]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Verification.Equivalence" "qcl.build.equivalence" Nothing Nothing

  , Todo 13 "Build Quipper Importer with source traceability" Pending High [12]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Backend.Quipper" "qcl.build.quipper" Nothing Nothing

  , Todo 14 "Build Simulator Backend" Pending High [13]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Backend.Simulator" "qcl.build.simulator" Nothing Nothing

  , Todo 15 "Build OpenQASM Backend" Pending High [14]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Backend.OpenQASM" "qcl.build.openqasm" Nothing Nothing

  , Todo 16 "Build Compiler Main driver" Pending High [15]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Compiler.Driver" "qcl.build.driver" Nothing Nothing

  , Todo 17 "Build 743-wire validation test fixture" Pending High [16]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Compiler.Fixture" "qcl.build.fixture" Nothing Nothing

  , Todo 18 "Verify full project builds and all tests pass" Pending High [17]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Compiler" "qcl.verify.all" Nothing Nothing
  ]

-- | Initialize todo state
initTodoState :: UTCTime -> TodoState
initTodoState now = TodoState
  { todos = Map.fromList [(todoId t, t) | t <- quantumCryptoLangTodos]
  , nextId = 19
  , lastUpdated = now
  , executionLog = []
  }

-- | Type for state monad operations
type TodoM a = State TodoState a

-- | Get a todo by ID
getTodo :: Int -> TodoM (Maybe Todo)
getTodo tid = do
  st <- get
  return $ Map.lookup tid (todos st)

-- | Resolve dependencies: can we execute this todo?
resolveDependencies :: Int -> TodoM DependencyResolution
resolveDependencies tid = do
  st <- get
  case Map.lookup tid (todos st) of
    Nothing -> return $ DependencyResolution False [tid] []
    Just t -> do
      let deps = dependencies t
      depStatuses <- mapM getTodo deps
      let completed = [d | (Just todo) <- depStatuses, todoId todo `elem` deps, status todo == Completed]
      let failed = [d | (Just todo) <- depStatuses, todoId todo `elem` deps, status todo == Failed]
      if length failed > 0
        then return $ DependencyResolution False failed []
        else if length completed == length deps
          then return $ DependencyResolution True [] [tid]
          else return $ DependencyResolution False (deps \\ completed) []
  where
    (\\\) = flip (filter . flip notElem)

-- | Get next executable todo
getNextTodo :: TodoM (Maybe Todo)
getNextTodo = do
  st <- get
  let pending = Map.filter (\t -> status t == Pending) (todos st)
  results <- mapM (\t -> (todoId t,) <$> resolveDependencies (todoId t)) (Map.elems pending)
  let executable = [t | (tid, res) <- results, canExecute res, Just t <- [Map.lookup tid (todos st)]]
  case executable of
    [] -> return Nothing
    (t:_) -> return $ Just t

-- | Mark todo as in progress
markInProgress :: Int -> TodoM ()
markInProgress tid = do
  st <- get
  case Map.lookup tid (todos st) of
    Nothing -> return ()
    Just t -> do
      now <- lift getCurrentTime
      let updated = t { status = InProgress, updatedAt = now }
      put $ st { todos = Map.insert tid updated (todos st), lastUpdated = now }
  where
    lift = id  -- Would use actual lift in StateT context

-- | Mark todo as completed
markCompleted :: Int -> UTCTime -> Maybe Double -> TodoM ()
markCompleted tid now execTime = do
  st <- get
  case Map.lookup tid (todos st) of
    Nothing -> return ()
    Just t -> do
      let updated = t
            { status = Completed
            , updatedAt = now
            , completedAt = Just now
            , executionTime = execTime
            }
      put $ st { todos = Map.insert tid updated (todos st), lastUpdated = now }

-- | Mark todo as failed
markFailed :: Int -> UTCTime -> String -> TodoM ()
markFailed tid now err = do
  st <- get
  case Map.lookup tid (todos st) of
    Nothing -> return ()
    Just t -> do
      let updated = t
            { status = Failed
            , updatedAt = now
            , error = Just err
            }
      put $ st { todos = Map.insert tid updated (todos st), lastUpdated = now }

-- | Get completion percentage
getCompletionPercentage :: TodoM Double
getCompletionPercentage = do
  st <- get
  let total = length (todos st)
  let completed = length $ filter (\t -> status t == Completed) (Map.elems (todos st))
  return $ if total == 0 then 0 else fromIntegral completed / fromIntegral total * 100

-- | Get all todos sorted by priority and ID
getAllTodos :: TodoM [Todo]
getAllTodos = do
  st <- get
  return $ sortBy (comparing todoId) (Map.elems (todos st))

-- | Get todos by status
getTodosByStatus :: TodoStatus -> TodoM [Todo]
getTodosByStatus s = do
  st <- get
  return $ filter (\t -> status t == s) (Map.elems (todos st))

-- | Create execution request for a todo
createExecutionRequest :: Todo -> String -> AuthContext -> TodoM ExecutionRequest
createExecutionRequest t correlId authCtx = do
  now <- lift getCurrentTime
  let deadline = Just $ addUTCTime 60 now  -- 60 second timeout
  return $ ExecutionRequest
    { requestId = "req-" ++ show (todoId t)
    , toolId = toolId t
    , operation = content t
    , arguments = Map.fromList
        [ ("module", moduleName t)
        , ("todo_id", show (todoId t))
        ]
    , priority = case priority t of
        Critical -> 4
        High -> 3
        Medium -> 2
        Low -> 1
    , deadline = deadline
    , authorizationContext = authCtx
    , correlationId = correlId
    }
  where
    lift = id

-- | Record execution in audit log
recordExecution :: ExecutionLog -> TodoM ()
recordExecution log = do
  st <- get
  put $ st { executionLog = log : executionLog st }

-- | Get execution history for a todo
getExecutionHistory :: Int -> TodoM [ExecutionLog]
getExecutionHistory tid = do
  st <- get
  return $ filter (\log -> logTodoId log == tid) (executionLog st)
