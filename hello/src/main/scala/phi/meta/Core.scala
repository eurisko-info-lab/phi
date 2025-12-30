package phi.meta

import Val.*
import Env.*
import Pat.*
import Expr.*
import Result.*

/**
 * Core: Pattern matching and evaluation (generated from meta.phi)
 * 
 * These are the fundamental semantic operations:
 * - match: (Pat, Val, Env) → Result
 * - eval: (Expr, Env) → Val
 */
object Core:

  // ==========================================================================
  // Pattern Matching
  // ==========================================================================
  
  /** Match a pattern against a value, extending the environment with bindings */
  def matchPat(pat: Pat, value: Val, env: Env): Result = pat match
    // Variable matches anything, binds it
    case PVar(name) =>
      ROk(value, Bind(name, value, env))
    
    // Constructor matches if names equal and args match
    case PCon(pname, pargs) =>
      value match
        case VCon(vname, vargs) if pname == vname && pargs.length == vargs.length =>
          matchArgs(pargs, vargs, env)
        case _ => RFail
    
    // Wildcard matches anything, binds nothing
    case PWild =>
      ROk(value, env)
    
    // Literal matches if equal
    case PLit(expected) =>
      if value == expected then ROk(value, env) else RFail
  
  /** Match a list of patterns against a list of values */
  def matchArgs(pats: List[Pat], vals: List[Val], env: Env): Result =
    (pats, vals) match
      case (Nil, Nil) => ROk(VList(Nil), env)
      case (p :: ps, v :: vs) =>
        matchPat(p, v, env) match
          case ROk(_, env1) => matchArgs(ps, vs, env1)
          case RFail => RFail
      case _ => RFail

  // ==========================================================================
  // Evaluation
  // ==========================================================================
  
  /** Evaluate an expression in an environment */
  def eval(expr: Expr, env: Env): Val = expr match
    // Variable lookup
    case EVar(name) =>
      lookup(name, env).getOrElse(VCon(name, Nil))
    
    // Constructor: evaluate args, build value
    case ECon(name, args) =>
      VCon(name, args.map(eval(_, env)))
    
    // Application: evaluate both, apply
    case EApp(func, arg) =>
      apply(eval(func, env), eval(arg, env), env)
    
    // Lambda: create closure (captured as VCon for simplicity)
    case ELam(param, body) =>
      // Represent closure as VCon("Closure", [param, body, env])
      // For now, just return a marker
      VCon("Closure", List(VStr(param), VCon("Body", Nil)))
    
    // Let: evaluate value, extend env, evaluate body
    case ELet(name, value, body) =>
      val v = eval(value, env)
      eval(body, Bind(name, v, env))
    
    // Match: evaluate scrutinee, try cases
    case EMatch(scrutinee, cases) =>
      val v = eval(scrutinee, env)
      tryCases(v, cases, env)
    
    // Case clause (shouldn't be evaluated directly)
    case ECase(_, _) =>
      VCon("Error", List(VStr("ECase evaluated directly")))
  
  /** Try each case until one matches */
  def tryCases(value: Val, cases: List[Expr], env: Env): Val =
    cases match
      case Nil => VCon("MatchError", List(value))
      case ECase(pat, body) :: rest =>
        matchPat(pat, value, env) match
          case ROk(_, env1) => eval(body, env1)
          case RFail => tryCases(value, rest, env)
      case _ :: rest => tryCases(value, rest, env)
  
  /** Apply a function value to an argument */
  def apply(func: Val, arg: Val, env: Env): Val =
    // For now, just return application as a value
    VCon("App", List(func, arg))
