-- | Env: Environment for variable bindings
module Phi.Meta.Env
  ( Env
  , empty
  , lookup
  , bind
  , extend
  , merge
  ) where

import Prelude hiding (lookup)
import Phi.Meta.Val (Val)

-- | Environment is a list of name-value bindings
type Env = [(String, Val)]

-- | Empty environment
empty :: Env
empty = []

-- | Look up a variable in the environment
lookup :: String -> Env -> Maybe Val
lookup _ [] = Nothing
lookup name ((n, v):rest)
  | name == n = Just v
  | otherwise = lookup name rest

-- | Bind a single variable
bind :: String -> Val -> Env -> Env
bind name val env = (name, val) : env

-- | Extend environment with multiple bindings
extend :: [(String, Val)] -> Env -> Env
extend bindings env = bindings ++ env

-- | Merge two environments (second takes precedence)
merge :: Env -> Env -> Env
merge env1 env2 = env2 ++ env1
