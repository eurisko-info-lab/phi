package phi.meta.gen

import phi.meta.{Val, Env, Pat, Expr}
import Val.*
import Env.*
import Pat.*
import Expr.*

/**
 * Show: Pretty printing (generated from meta.phi rule Show)
 * 
 * Each case of Val, Pat, Expr has its own show implementation.
 */
object Show:

  // ========== Val.show ==========
  
  extension (v: Val)
    def show: String = v match
      case c: VCon  => c.showCon
      case s: VStr  => s.showStr
      case i: VInt  => i.showInt
      case l: VList => l.showList

  // rule Show.con { VCon(name, Nil) ↦ name }
  // rule Show.con { VCon(name, args) ↦ name ++ "(" ++ args.map(show).mkString(", ") ++ ")" }
  extension (v: VCon)
    def showCon: String =
      if v.args.isEmpty then v.name
      else s"${v.name}(${v.args.map(_.show).mkString(", ")})"

  // rule Show.str { VStr(s) ↦ "\"" ++ s ++ "\"" }
  extension (v: VStr)
    def showStr: String = s"\"${v.s}\""

  // rule Show.int { VInt(n) ↦ n.toString }
  extension (v: VInt)
    def showInt: String = v.n.toString

  // rule Show.list { VList(elems) ↦ "[" ++ elems.map(show).mkString(", ") ++ "]" }
  extension (v: VList)
    def showList: String = s"[${v.elems.map(_.show).mkString(", ")}]"

  // ========== Pat.show ==========
  
  extension (p: Pat)
    def show: String = p match
      case v: PVar  => v.showPVar
      case c: PCon  => c.showPCon
      case PWild    => "_"
      case l: PLit  => l.showPLit

  extension (p: PVar)
    def showPVar: String = p.name

  extension (p: PCon)
    def showPCon: String =
      if p.args.isEmpty then p.name
      else s"${p.name}(${p.args.map(_.show).mkString(", ")})"

  extension (p: PLit)
    def showPLit: String = p.value.show

  // ========== Expr.show ==========
  
  extension (e: Expr)
    def show: String = e match
      case v: EVar   => v.name
      case c: ECon   => c.showECon
      case a: EApp   => a.showEApp
      case l: ELam   => l.showELam
      case t: ELet   => t.showELet
      case m: EMatch => m.showEMatch
      case c: ECase  => c.showECase

  extension (e: ECon)
    def showECon: String =
      if e.args.isEmpty then e.name
      else s"${e.name}(${e.args.map(_.show).mkString(", ")})"

  extension (e: EApp)
    def showEApp: String = s"(${e.func.show} ${e.arg.show})"

  extension (e: ELam)
    def showELam: String = s"λ${e.param}. ${e.body.show}"

  extension (e: ELet)
    def showELet: String = s"let ${e.name} = ${e.value.show} in ${e.body.show}"

  extension (e: EMatch)
    def showEMatch: String = s"match ${e.scrutinee.show} { ${e.cases.map(_.show).mkString("; ")} }"

  extension (e: ECase)
    def showECase: String = s"${e.pattern.show} => ${e.body.show}"

  // ========== Env.show ==========
  
  extension (env: Env)
    def show: String = env match
      case EmptyEnv => "{}"
      case Bind(name, value, rest) => s"{$name -> ${value.show}, ${rest.show}}"
