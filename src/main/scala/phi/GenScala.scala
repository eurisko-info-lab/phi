package phi

import java.nio.file.{Files, Paths}
import Val.*

/**
 * Generate Scala code from Phi language specs.
 * 
 * Usage: sbt "runMain phi.GenScala examples/stlc-nat.phi"
 * 
 * Builds Scala AST (as Val), then pretty-prints using bidirectional syntax.
 */
@main def GenScala(specFile: String): Unit =
  parseSpecWithInheritance(specFile) match
    case Left(err) =>
      System.err.println(s"Parse error: $err")
      sys.exit(1)
    case Right(spec) =>
      // Build Scala AST
      val scalaDefns = buildScalaAST(spec)
      
      // Pretty-print each definition
      println(s"// Generated from $specFile")
      println(s"package ${spec.name.toLowerCase}")
      println()
      for defn <- scalaDefns do
        println(showScalaDefn(defn))

/** Build Scala AST from Phi spec */
def buildScalaAST(spec: LangSpec): List[Val] =
  val sortDefns = spec.sorts.map(sort2Scala)
  val ctorDefns = spec.constructors.map(ctor2Scala)
  val xformDefns = spec.xforms.flatMap(x => xform2Scala(x, spec.rules))
  sortDefns ++ ctorDefns ++ xformDefns

/** Sort → SealedTrait(name, typeParams) */
def sort2Scala(sort: Sort): Val =
  VCon("SealedTrait", List(
    VCon(sort.name, Nil),  // String as nullary constructor
    VCon("nil", sort.typeParams.map(p => VCon(p, Nil)))  // List of type params
  ))

/** Constructor → CaseClass or CaseObject */
def ctor2Scala(ctor: Constructor): Val =
  val params = ctor.params.zipWithIndex.map { case ((nameOpt, ty), i) =>
    val argName = nameOpt.getOrElse(s"arg$i")
    VCon("Param", List(VCon(argName, Nil), type2Scala(ty)))
  }
  
  if params.isEmpty then
    VCon("CaseObject", List(
      VCon(ctor.name, Nil),
      VCon("TyName", List(VCon("SimpleName", List(VCon(ctor.returnSort, Nil)))))
    ))
  else
    VCon("CaseClassExtends", List(
      VCon(ctor.name, Nil),
      VCon("nil", Nil),  // no type params
      VCon("nil", params),
      VCon("TyName", List(VCon("SimpleName", List(VCon(ctor.returnSort, Nil)))))
    ))

/** LangType → Scala TypeRef */
def type2Scala(ty: LangType): Val = ty match
  case LangType.SortRef(name) => 
    VCon("TyName", List(VCon("SimpleName", List(VCon(name, Nil)))))
  case LangType.TypeApp(base, args) =>
    VCon("TyApp", List(
      VCon("SimpleName", List(VCon(base, Nil))),
      VCon("nil", args.map(type2Scala))
    ))
  case LangType.TypeVar(name) =>
    VCon("TyName", List(VCon("SimpleName", List(VCon(name, Nil)))))
  case LangType.Arrow(from, to) =>
    VCon("TyFunc", List(type2Scala(from), type2Scala(to)))
  case LangType.Product(a, b) =>
    VCon("TyTuple", List(VCon("nil", List(type2Scala(a), type2Scala(b)))))
  case LangType.ListOf(elem) =>
    VCon("TyApp", List(
      VCon("SimpleName", List(VCon("List", Nil))),
      VCon("nil", List(type2Scala(elem)))
    ))

/** Xform + Rules → MethodDef with match expression */
def xform2Scala(xform: XformSpec, rules: List[Rule]): Option[Val] =
  val forwardRules = rules.filter(_.name.startsWith(xform.name + "."))
  if forwardRules.isEmpty then None
  else
    val cases = forwardRules.flatMap(_.cases).map { rcase =>
      VCon("CaseSimple", List(
        pat2ScalaPat(rcase.lhs),
        pat2ScalaExpr(rcase.rhs)
      ))
    }
    Some(VCon("MethodDef", List(
      VCon(toCamelCase(xform.name) + "Forward", Nil),
      VCon("nil", Nil),  // no type params
      VCon("nil", List(VCon("Param", List(VCon("input", Nil), VCon("TyName", List(VCon("SimpleName", List(VCon(xform.source, Nil))))))))),
      VCon("TyName", List(VCon("SimpleName", List(VCon(xform.target, Nil))))),
      VCon("EMatch", List(
        VCon("EVar", List(VCon("input", Nil))),
        VCon("nil", cases)
      ))
    )))

/** MetaPattern → Scala Pattern */
def pat2ScalaPat(p: MetaPattern): Val = p match
  case MetaPattern.PVar(name) => VCon("PVar", List(VCon(name, Nil)))
  case MetaPattern.PCon(name, Nil) => VCon("PVar", List(VCon(name, Nil)))  // nullary as variable
  case MetaPattern.PCon(name, args) => 
    VCon("PCon", List(
      VCon("SimpleName", List(VCon(name, Nil))),
      VCon("nil", args.map(pat2ScalaPat))
    ))
  case MetaPattern.PApp(f, a) =>
    VCon("PCon", List(
      VCon("SimpleName", List(VCon("App", Nil))),
      VCon("nil", List(pat2ScalaPat(f), pat2ScalaPat(a)))
    ))
  case MetaPattern.PSubst(body, v, repl) =>
    pat2ScalaPat(body)  // Substitution patterns need special handling

/** MetaPattern → Scala Expr */
def pat2ScalaExpr(p: MetaPattern): Val = p match
  case MetaPattern.PVar(name) => VCon("EVar", List(VCon(name, Nil)))
  case MetaPattern.PCon(name, Nil) => VCon("EVar", List(VCon(name, Nil)))
  case MetaPattern.PCon(name, args) =>
    VCon("EApp", List(
      VCon("EVar", List(VCon(name, Nil))),
      VCon("nil", args.map(pat2ScalaExpr))
    ))
  case MetaPattern.PApp(f, a) =>
    VCon("EApp", List(
      pat2ScalaExpr(f),
      VCon("nil", List(pat2ScalaExpr(a)))
    ))
  case MetaPattern.PSubst(body, v, repl) =>
    VCon("EApp", List(
      VCon("EVar", List(VCon("subst", Nil))),
      VCon("nil", List(pat2ScalaExpr(body), VCon("ELit", List(VCon("LitString", List(VCon(v, Nil))))), pat2ScalaExpr(repl)))
    ))

def toCamelCase(s: String): String =
  val parts = s.split("[._]")
  parts.head.toLowerCase + parts.tail.map(_.capitalize).mkString

// =============================================================================
// Bidirectional Pretty-Printer for Scala AST
// =============================================================================
// These correspond to the syntax declarations in scala.phi (the <-> comments)

def showScalaDefn(v: Val): String = v match
  // SealedTrait(n, tps) <-> "sealed trait " n tps
  case VCon("SealedTrait", List(name, tps)) =>
    val tpStr = showTypeParams(tps)
    s"sealed trait ${showName(name)}$tpStr"
  
  // CaseClass(n, tps, ps) <-> "case class " n tps "(" ps ")"
  case VCon("CaseClass", List(name, tps, ps)) =>
    val tpStr = showTypeParams(tps)
    val psStr = showParams(ps)
    s"case class ${showName(name)}$tpStr($psStr)"
  
  // CaseClassExtends(n, tps, ps, ext) <-> "case class " n tps "(" ps ") extends " ext
  case VCon("CaseClassExtends", List(name, tps, ps, ext)) =>
    val tpStr = showTypeParams(tps)
    val psStr = showParams(ps)
    s"case class ${showName(name)}$tpStr($psStr) extends ${showTypeRef(ext)}"
  
  // CaseObject(n, ext) <-> "case object " n " extends " ext
  case VCon("CaseObject", List(name, ext)) =>
    s"case object ${showName(name)} extends ${showTypeRef(ext)}"
  
  // MethodDef(n, tps, ps, ret, body) <-> "def " n tps ps ": " ret " = " body
  case VCon("MethodDef", List(name, tps, ps, ret, body)) =>
    val tpStr = showTypeParams(tps)
    val psStr = showParams(ps)
    s"def ${showName(name)}$tpStr($psStr): ${showTypeRef(ret)} = ${showExpr(body)}"
  
  case other => s"/* unknown: ${other.show} */"

def showName(v: Val): String = v match
  case VCon(name, Nil) => name
  case VCon("SimpleName", List(VCon(n, Nil))) => n
  case VCon("QualName", List(base, VCon(n, Nil))) => s"${showName(base)}.$n"
  case other => other.show

def showTypeParams(v: Val): String = v match
  case VCon("nil", Nil) => ""
  case VCon("nil", tps) if tps.nonEmpty => s"[${tps.map(showName).mkString(", ")}]"
  case _ => ""

def showParams(v: Val): String = v match
  case VCon("nil", Nil) => ""
  case VCon("nil", ps) => ps.map(showParam).mkString(", ")
  case _ => ""

def showParam(v: Val): String = v match
  case VCon("Param", List(name, ty)) => s"${showName(name)}: ${showTypeRef(ty)}"
  case VCon("ParamDefault", List(name, ty, default)) => s"${showName(name)}: ${showTypeRef(ty)} = ${showExpr(default)}"
  case other => other.show

def showTypeRef(v: Val): String = v match
  case VCon("TyName", List(name)) => showName(name)
  case VCon("TyApp", List(name, args)) => 
    val argsStr = args match
      case VCon("nil", as) => as.map(showTypeRef).mkString(", ")
      case _ => ""
    s"${showName(name)}[$argsStr]"
  case VCon("TyFunc", List(from, to)) => s"${showTypeRef(from)} => ${showTypeRef(to)}"
  case VCon("TyTuple", List(VCon("nil", elems))) => s"(${elems.map(showTypeRef).mkString(", ")})"
  case VCon("TyUnit", Nil) => "Unit"
  case other => other.show

def showExpr(v: Val): String = v match
  case VCon("EVar", List(name)) => showName(name)
  case VCon("ELit", List(lit)) => showLiteral(lit)
  case VCon("EApp", List(f, VCon("nil", args))) => 
    s"${showExpr(f)}(${args.map(showExpr).mkString(", ")})"
  case VCon("ESelect", List(e, VCon(field, Nil))) => s"${showExpr(e)}.$field"
  case VCon("EMatch", List(scrutinee, VCon("nil", cases))) =>
    val casesStr = cases.map(showCase).mkString("\n    ")
    s"${showExpr(scrutinee)} match {\n    $casesStr\n  }"
  case VCon("EIf", List(cond, thn, els)) =>
    s"if ${showExpr(cond)} then ${showExpr(thn)} else ${showExpr(els)}"
  case VCon("ELambda", List(VCon("nil", ps), body)) =>
    s"(${ps.map(showParam).mkString(", ")}) => ${showExpr(body)}"
  case other => other.show

def showLiteral(v: Val): String = v match
  case VCon("LitString", List(VCon(s, Nil))) => s"\"$s\""
  case VCon("LitInt", List(VCon(n, Nil))) => n
  case VCon("LitTrue", Nil) => "true"
  case VCon("LitFalse", Nil) => "false"
  case VCon("LitNil", Nil) => "Nil"
  case other => other.show

def showCase(v: Val): String = v match
  case VCon("CaseSimple", List(pat, body)) => s"case ${showPattern(pat)} => ${showExpr(body)}"
  case VCon("CaseGuard", List(pat, guard, body)) => s"case ${showPattern(pat)} if ${showExpr(guard)} => ${showExpr(body)}"
  case other => other.show

def showPattern(v: Val): String = v match
  case VCon("PVar", List(name)) => showName(name)
  case VCon("PWildcard", Nil) => "_"
  case VCon("PLit", List(lit)) => showLiteral(lit)
  case VCon("PCon", List(name, VCon("nil", args))) => 
    s"${showName(name)}(${args.map(showPattern).mkString(", ")})"
  case VCon("PTuple", List(VCon("nil", elems))) => s"(${elems.map(showPattern).mkString(", ")})"
  case other => other.show
