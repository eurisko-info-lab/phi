-- | Eval: Expression evaluation
-- Generated structure based on meta2haskell.phi GenEval transform
module Phi.Meta.Eval
  ( eval
  , tryCases
  , applyVal
  ) where

import Prelude hiding (lookup)
import Phi.Meta.Val
import Phi.Meta.Env as Env
import Phi.Meta.Pat
import Phi.Meta.Expr
import Phi.Meta.Result
import Phi.Meta.Match (matchWith)

-- | Evaluate an expression in an environment
eval :: Expr -> Env -> Val

-- rule Eval.var { EVar(name) ↦ Lookup(name, env) }
eval (EVar name) env = 
  case Env.lookup name env of
    Just v  -> v
    Nothing -> VCon name []  -- Treat unbound as constructor

-- rule Eval.con { ECon(name, args) ↦ VCon(name, Eval.list(args, env)) }
eval (ECon name args) env = VCon name (map (`eval` env) args)

-- rule Eval.app { EApp(func, arg) ↦ Apply(Eval(func), Eval(arg)) }
eval (EApp func arg) env = applyVal (eval func env) (eval arg env) env

-- rule Eval.lam { ELam(param, body) ↦ VClosure(param, body, env) }
eval (ELam param body) env = VClosure param (exprToVal body) env

-- rule Eval.let { ELet(name, value, body) ↦ Eval(body, Bind(name, Eval(value), env)) }
eval (ELet name value body) env = 
  let v = eval value env
  in eval body (Env.bind name v env)

-- rule Eval.match { EMatch(scrutinee, cases) ↦ TryCases(Eval(scrutinee), cases, env) }
eval (EMatch scrutinee cases) env = tryCases (eval scrutinee env) cases env

-- rule Eval.case { ECase(pat, body) ↦ Error(...) }
-- Cases should only be evaluated as part of EMatch
eval (ECase _ _) _ = VCon "Error" [VStr "ECase evaluated outside EMatch"]

-- rule Eval.lit { ELit(n) ↦ VInt(n) }
eval (ELit n) _ = VInt n

-- rule Eval.str { EStr(s) ↦ VStr(s) }
eval (EStr s) _ = VStr s

-- rule Eval.list { EList(exprs) ↦ VList(Eval.list(exprs, env)) }
eval (EList exprs) env = VList (map (`eval` env) exprs)

-- rule Eval.if { EIf(cond, then_, else_) ↦ ... }
eval (EIf cond thenExpr elseExpr) env =
  case eval cond env of
    VCon "True" []  -> eval thenExpr env
    VCon "False" [] -> eval elseExpr env
    _               -> VCon "Error" [VStr "Non-boolean condition"]

-- | Try each case until one matches (part of EMatch semantics)
tryCases :: Val -> [Expr] -> Env -> Val
tryCases value [] _ = VCon "MatchError" [value]
tryCases value (ECase pat body : rest) env =
  case matchWith pat value env of
    ROk _ env' -> eval body env'
    RFail      -> tryCases value rest env
tryCases value (_ : rest) env = tryCases value rest env

-- | Apply a function value to an argument
applyVal :: Val -> Val -> Env -> Val
applyVal (VClosure param body capturedEnv) arg _ =
  eval (valToExpr body) (Env.bind param arg capturedEnv)
applyVal func arg _ = VCon "App" [func, arg]

-- | Convert expression to value (for closure storage)
exprToVal :: Expr -> Val
exprToVal (EVar name) = VCon "EVar" [VStr name]
exprToVal (ECon name args) = VCon "ECon" [VStr name, VList (map exprToVal args)]
exprToVal (EApp f a) = VCon "EApp" [exprToVal f, exprToVal a]
exprToVal (ELam p b) = VCon "ELam" [VStr p, exprToVal b]
exprToVal (ELet n v b) = VCon "ELet" [VStr n, exprToVal v, exprToVal b]
exprToVal (EMatch s cs) = VCon "EMatch" [exprToVal s, VList (map exprToVal cs)]
exprToVal (ECase p b) = VCon "ECase" [patToVal p, exprToVal b]
exprToVal (ELit n) = VInt n
exprToVal (EStr s) = VStr s
exprToVal (EList es) = VList (map exprToVal es)
exprToVal (EIf c t e) = VCon "EIf" [exprToVal c, exprToVal t, exprToVal e]

-- | Convert value back to expression (for closure evaluation)
valToExpr :: Val -> Expr
valToExpr (VCon "EVar" [VStr name]) = EVar name
valToExpr (VCon "ECon" [VStr name, VList args]) = ECon name (map valToExpr args)
valToExpr (VCon "EApp" [f, a]) = EApp (valToExpr f) (valToExpr a)
valToExpr (VCon "ELam" [VStr p, b]) = ELam p (valToExpr b)
valToExpr (VCon "ELet" [VStr n, v, b]) = ELet n (valToExpr v) (valToExpr b)
valToExpr (VCon "EMatch" [s, VList cs]) = EMatch (valToExpr s) (map valToExpr cs)
valToExpr (VCon "ECase" [p, b]) = ECase (valToPat p) (valToExpr b)
valToExpr (VInt n) = ELit n
valToExpr (VStr s) = EStr s
valToExpr (VList vs) = EList (map valToExpr vs)
valToExpr (VCon "EIf" [c, t, e]) = EIf (valToExpr c) (valToExpr t) (valToExpr e)
valToExpr v = ECon (show v) []  -- Fallback

-- | Convert pattern to value
patToVal :: Pat -> Val
patToVal (PVar name) = VCon "PVar" [VStr name]
patToVal PWild = VCon "PWild" []
patToVal (PCon name pats) = VCon "PCon" [VStr name, VList (map patToVal pats)]
patToVal (PLit s) = VCon "PLit" [VStr s]
patToVal (PList pats) = VCon "PList" [VList (map patToVal pats)]
patToVal (PAs name pat) = VCon "PAs" [VStr name, patToVal pat]

-- | Convert value to pattern
valToPat :: Val -> Pat
valToPat (VCon "PVar" [VStr name]) = PVar name
valToPat (VCon "PWild" []) = PWild
valToPat (VCon "PCon" [VStr name, VList pats]) = PCon name (map valToPat pats)
valToPat (VCon "PLit" [VStr s]) = PLit s
valToPat (VCon "PList" [VList pats]) = PList (map valToPat pats)
valToPat (VCon "PAs" [VStr name, pat]) = PAs name (valToPat pat)
valToPat _ = PWild  -- Fallback
