package phi.meta.gen

import phi.phi.MetaPattern
import phi.meta.{Val, Env, Pat, Expr, Result}
import Val.*
import Env.*
import Pat.*
import Expr.*
import Result.*

/** Interp: Generated interpreter dispatch from meta.phi xforms  */
object Interp:

  // ========== Apply transform by name ==========
  def applyXform(xformName: String, value: Val): Option[Val] = xformName match
    case "Match" => applyMatch(value)
    case "Eval" => applyEval(value)
    case "Show" => applyShow(value)
    case "Subst" => applySubst(value)
    case _ => None

  // ========== Match ==========
  def applyMatch(value: Val): Option[Val] = value match
    case _ => None

  // ========== Eval ==========
  def applyEval(value: Val): Option[Val] = value match
    case _ => None

  // ========== Show ==========
  def applyShow(value: Val): Option[Val] = value match
    case _ => None

  // ========== Subst ==========
  def applySubst(value: Val): Option[Val] = value match
    case _ => None

  // ========== Pattern Matching (MetaPattern → Val → bindings) ==========
  def matchPattern(pattern: MetaPattern, value: Val): Option[Map[String, Val]] = pattern match
    case MetaPattern.PVar(name) => Some(Map(name -> value))
    case MetaPattern.PCon(name, args) => matchPCon(name, args, value)
    case MetaPattern.PApp(func, arg) => None
    case MetaPattern.PSubst(_, _, _) => None

  // Match constructor pattern against value
  def matchPCon(name: String, args: List[MetaPattern], value: Val): Option[Map[String, Val]] = value match
    case VCon(vname, vargs) if vname == name && args.length == vargs.length => matchArgs(args, vargs)
    case VStr(s) if args.isEmpty && name == s => Some(Map.empty)
    case _ => None

  // Match list of patterns against list of values
  def matchArgs(pats: List[MetaPattern], vals: List[Val]): Option[Map[String, Val]] =(pats, vals) match
    case(Nil, Nil) => Some(Map.empty)
    case(p :: ps, v :: vs) => matchPattern(p, v).flatMap(b1 => matchArgs(ps, vs).map(b2 => b1 ++ b2))
    case _ => None

  // ========== Substitution (MetaPattern → bindings → Val) ==========
  def substitute(expr: MetaPattern, bindings: Map[String, Val]): Val = expr match
    case MetaPattern.PVar(name) => bindings.get(name).getOrElse(VCon(name, Nil))
    case MetaPattern.PCon(name, args) => substPCon(name, args, bindings)
    case MetaPattern.PApp(func, arg) => substPApp(func, arg, bindings)
    case MetaPattern.PSubst(body, varName, replacement) => substPSubst(body, varName, replacement, bindings)

  // Substitute in constructor - handles special cases
  def substPCon(name: String, args: List[MetaPattern], bindings: Map[String, Val]): Val = {
    val transformedArgs = args.map(a => substitute(a, bindings))
    if name == "StrConcat" then VStr(transformedArgs.map(extractString).mkString("")) else if name == "" then VStr(transformedArgs.map(extractString).mkString("")) else VCon(name, transformedArgs)
  }

  // Substitute in application
  def substPApp(func: MetaPattern, arg: MetaPattern, bindings: Map[String, Val]): Val = VCon("App", List(substitute(func, bindings), substitute(arg, bindings)))

  // Substitute with local binding
  def substPSubst(body: MetaPattern, varName: String, replacement: MetaPattern, bindings: Map[String, Val]): Val = {
    val replacementVal = substitute(replacement, bindings)
    substitute(body, bindings + (varName -> replacementVal))
  }

  // Extract string from Val
  def extractString(v: Val): String = v match
    case VStr(s) => s
    case VCon(s, Nil) => s
    case _ => v.toString
