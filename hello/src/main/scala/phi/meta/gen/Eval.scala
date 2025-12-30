package phi.meta.gen

import phi.meta.{Val, Env, Pat, Expr, Result}
import Val.*
import Env.*
import Pat.*
import Expr.*
import Result.*

import phi.meta.gen.Match.matchWith

/**
 * Eval: Expression evaluation (generated from meta.phi rule Eval)
 * 
 * Each case of Expr has its own eval implementation.
 * The dispatcher in Core calls expr.eval(env).
 */
object Eval:
  
  extension (expr: Expr)
    def eval(env: Env): Val = expr match
      case e: EVar   => e.evalVar(env)
      case e: ECon   => e.evalCon(env)
      case e: EApp   => e.evalApp(env)
      case e: ELam   => e.evalLam(env)
      case e: ELet   => e.evalLet(env)
      case e: EMatch => e.evalMatch(env)
      case e: ECase  => e.evalCase(env)

  // rule Eval.var { EVar(name) ↦ lookup(name, env) | VCon(name, Nil) }
  extension (e: EVar)
    def evalVar(env: Env): Val =
      lookup(e.name, env).getOrElse(VCon(e.name, Nil))

  // rule Eval.con { ECon(name, args) ↦ VCon(name, args.map(eval)) }
  extension (e: ECon)
    def evalCon(env: Env): Val =
      VCon(e.name, e.args.map(_.eval(env)))

  // rule Eval.app { EApp(func, arg) ↦ apply(eval(func), eval(arg)) }
  extension (e: EApp)
    def evalApp(env: Env): Val =
      applyVal(e.func.eval(env), e.arg.eval(env), env)

  // rule Eval.lam { ELam(param, body) ↦ VClosure(param, body, env) }
  extension (e: ELam)
    def evalLam(env: Env): Val =
      VCon("Closure", List(VStr(e.param), VCon("Body", Nil)))

  // rule Eval.let { ELet(name, value, body) ↦ eval(body, Bind(name, eval(value), env)) }
  extension (e: ELet)
    def evalLet(env: Env): Val =
      val v = e.value.eval(env)
      e.body.eval(Bind(e.name, v, env))

  // rule Eval.match { EMatch(scrutinee, cases) ↦ tryCases(eval(scrutinee), cases) }
  extension (e: EMatch)
    def evalMatch(env: Env): Val =
      val v = e.scrutinee.eval(env)
      tryCases(v, e.cases, env)

  // rule Eval.case { ECase(_, _) ↦ Error("ECase evaluated directly") }
  extension (e: ECase)
    def evalCase(env: Env): Val =
      VCon("Error", List(VStr("ECase evaluated directly")))

  // ==========================================================================
  // Runtime Support (generated helpers for Eval)
  // ==========================================================================
  
  /** Try each case until one matches (part of EMatch semantics) */
  def tryCases(value: Val, cases: List[Expr], env: Env): Val =
    cases match
      case Nil => VCon("MatchError", List(value))
      case ECase(pat, body) :: rest =>
        pat.matchWith(value, env) match
          case ROk(_, env1) => body.eval(env1)
          case RFail => tryCases(value, rest, env)
      case _ :: rest => tryCases(value, rest, env)
  
  /** Apply a function value to an argument */
  def applyVal(func: Val, arg: Val, env: Env): Val =
    // For now, just return application as a value
    VCon("App", List(func, arg))
