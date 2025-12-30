package phi.meta.gen

import phi.meta.{Val, Env, Pat, Result}
import Val.*
import Env.*
import Pat.*
import Result.*

/** Match: Pattern matching (generated from meta.phi Match xform)  */
object Match:
  extension(pat: Pat)
    def matchWith(value: Val, env: Env): Result = pat match
      case p: PVar => p.matchVar(value, env)
      case p: PCon => p.matchCon(value, env)
      case PWild => matchWild(value, env)
      case p: PLit => p.matchLit(value, env)

  // rule Match.var { PVar(name) ↦ ROk(val, Bind(name, val, env)) }
  extension(p: PVar)
    def matchVar(value: Val, env: Env): Result = ROk(value, Bind(p.name, value, env))
  // rule Match.con { PCon(name, pats) ↦ Match.args(pats, args, env) }
  extension(p: PCon)
    def matchCon(value: Val, env: Env): Result = value match
      case VCon(vname, vargs) if p.name == vname && p.args.length == vargs.length => matchArgs(p.args, vargs, env)
      case _ => RFail
  // rule Match.wild { PWild ↦ ROk(val, env) }
  def matchWild(value: Val, env: Env): Result = ROk(value, env)
  // rule Match.lit { PLit(expected) ↦ ROk(val, env) }
  extension(p: PLit)
    def matchLit(value: Val, env: Env): Result = if value == p.value then ROk(value, env) else RFail

  /** Match a list of patterns against a list of values  */
  def matchArgs(pats: List[Pat], vals: List[Val], env: Env): Result =(pats, vals) match
    case(Nil, Nil) => ROk(VList(Nil), env)
    case(p :: ps, v :: vs) => p.matchWith(v, env) match
      case ROk(_, env1) => matchArgs(ps, vs, env1)
      case RFail => RFail
    case _ => RFail
