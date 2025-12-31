-- | Expr: Expression types
module Phi.Meta.Expr
  ( Expr(..)
  ) where

import Phi.Meta.Pat (Pat)

-- | Expressions
data Expr
  = EVar String             -- ^ Variable reference
  | ECon String [Expr]      -- ^ Constructor application
  | EApp Expr Expr          -- ^ Function application
  | ELam String Expr        -- ^ Lambda abstraction
  | ELet String Expr Expr   -- ^ Let binding: let x = e1 in e2
  | EMatch Expr [Expr]      -- ^ Pattern match: match e with cases
  | ECase Pat Expr          -- ^ Single case: pattern => body
  | ELit Int                -- ^ Integer literal
  | EStr String             -- ^ String literal
  | EList [Expr]            -- ^ List literal
  | EIf Expr Expr Expr      -- ^ Conditional: if c then t else f
  deriving (Eq, Show)
