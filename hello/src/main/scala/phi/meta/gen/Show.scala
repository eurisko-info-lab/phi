package phi.meta.gen

import phi.meta.{Val, Env, Pat, Expr, Result}
import Val.*
import Env.*
import Pat.*
import Expr.*
import Result.*

/** Show: Pretty printing (generated from meta.phi Show xform)  */
object Show:

  // ========== Val.show ==========
  extension(v: Val)
    def show: String = v match
      case c: VCon => c.showCon
      case s: VStr => s.showStr
      case i: VInt => i.showInt
      case l: VList => l.showList

  // ========== Pat.show ==========
  extension(p: Pat)
    def show: String = p match
      case v: PVar => v.showPVar
      case c: PCon => c.showPCon
      case PWild => "_"
      case l: PLit => l.showPLit

  // ========== Expr.show ==========
  extension(e: Expr)
    def show: String = e match
      case v: EVar => v.name
      case c: ECon => c.showECon
      case a: EApp => a.showEApp
      case l: ELam => l.showELam
      case t: ELet => t.showELet
      case m: EMatch => m.showEMatch
      case cs: ECase => cs.showECase

  // ========== Env.show ==========
  extension(env: Env)
    def show: String = env match
      case EmptyEnv => "{}"
      case b: Bind => b.showBind

  // ========== Result.show ==========
  extension(r: Result)
    def show: String = r match
      case ok: ROk => ok.showOk
      case RFail => "RFail"
  // rule Show.vcon { VCon(name, args) ↦ ShowCon(...) }
  extension(self: VCon)
    def showCon: String = if self.args.isEmpty then self.name else self.name ++ "(" ++ self.args.map(_.show).mkString(", ") ++ ")"
  // rule Show.vstr { VStr(s) ↦ ShowStr(s) }
  extension(self: VStr)
    def showStr: String = "\"" ++ self.s ++ "\""
  // rule Show.vint { VInt(n) ↦ Show.int(n) }
  extension(self: VInt)
    def showInt: String = self.n.toString
  // rule Show.vlist { VList(elems) ↦ [...] }
  extension(self: VList)
    def showList: String = "[" ++ self.elems.map(_.show).mkString(", ") ++ "]"
  // rule Show.pvar { PVar(name) ↦ name }
  extension(self: PVar)
    def showPVar: String = self.name
  // rule Show.pcon { PCon(name, args) ↦ ShowCon(...) }
  extension(self: PCon)
    def showPCon: String = if self.args.isEmpty then self.name else self.name ++ "(" ++ self.args.map(_.show).mkString(", ") ++ ")"

  // rule Show.plit { PLit(value) ↦ Show.forward(value) }
  extension(self: PLit)
    def showPLit: String = self.value.show

  // rule Show.econ { ECon(name, args) ↦ ShowCon(...) }
  extension(self: ECon)
    def showECon: String = if self.args.isEmpty then self.name else self.name ++ "(" ++ self.args.map(_.show).mkString(", ") ++ ")"
  // rule Show.eapp { EApp(func, arg) ↦ Concat(...) }
  extension(self: EApp)
    def showEApp: String = "(" ++ self.func.show ++ " " ++ self.arg.show ++ ")"
  // rule Show.elam { ELam(param, body) ↦ Concat(...) }
  extension(self: ELam)
    def showELam: String = "λ" ++ self.param ++ ". " ++ self.body.show
  // rule Show.elet { ELet(name, value, body) ↦ Concat(...) }
  extension(self: ELet)
    def showELet: String = "let " ++ self.name ++ " = " ++ self.value.show ++ " in " ++ self.body.show
  // rule Show.ematch { EMatch(scrutinee, cases) ↦ Concat(...) }
  extension(self: EMatch)
    def showEMatch: String = "match " ++ self.scrutinee.show ++ " { " ++ self.cases.map(_.show).mkString("; ") ++ " }"
  // rule Show.ecase { ECase(pattern, body) ↦ Concat(...) }
  extension(self: ECase)
    def showECase: String = self.pattern.show ++ " => " ++ self.body.show

  // rule Show.bind { Bind(name, value, rest) ↦ Concat(...) }
  extension(self: Bind)
    def showBind: String = "{" ++ self.name ++ " -> " ++ self.value.show ++ ", " ++ self.rest.show ++ "}"
  // rule Show.ok { ROk(value, env) ↦ Concat(...) }
  extension(self: ROk)
    def showOk: String = "ROk(" ++ self.value.show ++ ", " ++ self.env.show ++ ")"

