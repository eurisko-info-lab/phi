package phi.meta

import Val.*
import Env.*
import Pat.*
import Expr.*
import Result.*

// Import extension methods from gen/
import phi.meta.gen.Match.matchWith
import phi.meta.gen.Eval.eval

/**
 * Core: Semantic dispatch layer
 * 
 * This is the thin coordination layer that knows only about types.
 * All implementations live in gen/:
 * - gen/Match.scala: Pattern matching (Pat.matchWith)
 * - gen/Eval.scala:  Expression evaluation (Expr.eval)
 * - gen/Show.scala:  Pretty printing (Val.show, Pat.show, Expr.show)
 * 
 * Each operation is an "extension point" - a separate file that can be
 * generated from .phi rules independently.
 */
object Core:

  /** Match a pattern against a value, extending the environment with bindings */
  def matchPat(pat: Pat, value: Val, env: Env): Result =
    pat.matchWith(value, env)

  /** Evaluate an expression in an environment */
  def eval(expr: Expr, env: Env): Val =
    expr.eval(env)
