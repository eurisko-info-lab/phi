-- | Pat: Pattern matching
module Phi.Meta.Pat
  ( Pat(..)
  ) where

-- | Patterns for matching values
data Pat
  = PVar String           -- ^ Variable pattern: binds any value
  | PWild                 -- ^ Wildcard: matches anything, no binding
  | PCon String [Pat]     -- ^ Constructor pattern: matches constructor with args
  | PLit String           -- ^ Literal pattern: matches exact value
  | PList [Pat]           -- ^ List pattern: matches list structure
  | PAs String Pat        -- ^ As-pattern: name@pat
  deriving (Eq, Show)
