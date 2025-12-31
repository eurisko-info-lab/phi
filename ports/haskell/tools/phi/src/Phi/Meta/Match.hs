-- | Match: Pattern matching implementation
-- Generated structure based on meta2haskell.phi GenMatch transform
module Phi.Meta.Match
  ( matchWith
  , matchArgs
  ) where

import Prelude hiding (lookup)
import Phi.Meta.Val
import Phi.Meta.Env
import Phi.Meta.Pat
import Phi.Meta.Result

-- | Match a pattern against a value in an environment
matchWith :: Pat -> Val -> Env -> Result

-- rule Match.var { PVar(name), value, env ↦ Ok(value, Bind(name, value, env)) }
matchWith (PVar name) value env = ROk value (bind name value env)

-- rule Match.wildcard { PWild, value, env ↦ Ok(value, env) }
matchWith PWild value env = ROk value env

-- rule Match.con { PCon(name, pats), VCon(name, vals), env ↦ ... }
matchWith (PCon pn pats) (VCon vn vals) env
  | pn == vn  = matchArgs pats vals env
  | otherwise = RFail

-- rule Match.lit { PLit(expected), VStr(actual), env ↦ ... }
matchWith (PLit expected) (VStr actual) env
  | expected == actual = ROk (VStr actual) env
  | otherwise = RFail

matchWith (PLit expected) (VInt actual) env
  | expected == show actual = ROk (VInt actual) env
  | otherwise = RFail

-- rule Match.list { PList(pats), VList(vals), env ↦ ... }
matchWith (PList pats) (VList vals) env
  | length pats == length vals = matchArgs pats vals env
  | otherwise = RFail

-- rule Match.as { PAs(name, pat), value, env ↦ ... }
matchWith (PAs name pat) value env =
  case matchWith pat value env of
    ROk v env' -> ROk v (bind name value env')
    RFail -> RFail

-- Fallback: no match
matchWith _ _ _ = RFail

-- | Match multiple patterns against multiple values
matchArgs :: [Pat] -> [Val] -> Env -> Result
matchArgs [] [] env = ROk (VCon "Unit" []) env
matchArgs (p:ps) (v:vs) env =
  case matchWith p v env of
    ROk _ env' -> matchArgs ps vs env'
    RFail -> RFail
matchArgs _ _ _ = RFail
