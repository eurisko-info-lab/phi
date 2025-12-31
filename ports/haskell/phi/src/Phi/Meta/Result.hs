-- | Result: Pattern matching result
module Phi.Meta.Result
  ( Result(..)
  , isOk
  , isFail
  ) where

import Phi.Meta.Val (Val)
import Phi.Meta.Env (Env)

-- | Result of pattern matching
data Result
  = ROk Val Env   -- ^ Match succeeded: matched value + extended environment
  | RFail         -- ^ Match failed
  deriving (Eq, Show)

-- | Check if result is successful
isOk :: Result -> Bool
isOk (ROk _ _) = True
isOk RFail = False

-- | Check if result is failure
isFail :: Result -> Bool
isFail = not . isOk
