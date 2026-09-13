{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Abstract Syntax Tree module
--
-- Re-exports AST types from QCL.Syntax.Parser for convenience.
-- All AST node types (Expr, Statement, Declaration, Program, etc.)
-- are defined in the Parser module and re-exported here.

module QCL.Syntax.AST
  ( -- * Expressions
    Expr(..)
  , Op(..)
  , UnaryOp(..)

    -- * Statements
  , Statement(..)
  , Basis(..)

    -- * Declarations
  , Declaration(..)
  , RegisterDecl(..)
  , RegisterType(..)
  , GateSpec(..)
  , CircuitDecl(..)
  , FunctionDecl(..)

    -- * Program
  , Program(..)

    -- * Parse errors
  , ParseError(..)
  ) where

import QCL.Syntax.Parser
