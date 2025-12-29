package phi

import java.nio.file.{Files, Paths, Path}
import Val.*

// =============================================================================
// Parse Phi type syntax into LangType
// =============================================================================
// Handles: Token, Token*, (A × B), A → B, List[A]

def parsePhiType(s: String): LangType =
  val trimmed = s.trim
  
  // Handle star types: A* → List[A]
  if trimmed.endsWith("*") then
    LangType.ListOf(parsePhiType(trimmed.dropRight(1)))
  
  // Handle arrow types: A → B
  else if trimmed.contains("→") then
    val parts = trimmed.split("→", 2).map(_.trim)
    LangType.Arrow(parsePhiType(parts(0)), parsePhiType(parts(1)))
  
  // Handle product types: (A × B) or ((A × B) × C)
  else if trimmed.startsWith("(") && trimmed.endsWith(")") then
    val inner = trimmed.drop(1).dropRight(1).trim
    // Find the × at the right nesting level
    var depth = 0
    var splitIdx = -1
    for i <- inner.indices if splitIdx < 0 do
      inner(i) match
        case '(' => depth += 1
        case ')' => depth -= 1
        case '×' if depth == 0 => splitIdx = i
        case _ =>
    if splitIdx > 0 then
      val left = inner.take(splitIdx).trim
      val right = inner.drop(splitIdx + 1).trim
      LangType.Product(parsePhiType(left), parsePhiType(right))
    else
      // Just parenthesized, recurse
      parsePhiType(inner)
  
  // Handle type application: List[A]
  else if trimmed.contains("[") then
    val baseEnd = trimmed.indexOf('[')
    val base = trimmed.take(baseEnd)
    val argsStr = trimmed.drop(baseEnd + 1).dropRight(1)
    val args = argsStr.split(",").map(_.trim).map(parsePhiType).toList
    LangType.TypeApp(base, args)
  
  // Simple name
  else
    LangType.SortRef(trimmed)

/** Convert dotted names to camelCase: Parse.forward → parseForward */
def toCamelCase(s: String): String =
  val parts = s.split("[._]")
  parts.head.toLowerCase + parts.tail.map(_.capitalize).mkString

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

/**
 * Generate Scala code from Phi language specs.
 * 
 * Usage: sbt "runMain phi.GenScala examples/stlc-nat.phi"
 * 
 * Builds Scala AST (as Val), then pretty-prints using bidirectional syntax.
 * Output goes to ./tmp/<language>.scala
 */
@main def GenScala(specFile: String): Unit =
  parseSpecWithInheritance(specFile) match
    case Left(err) =>
      System.err.println(s"Parse error: $err")
      sys.exit(1)
    case Right(spec) =>
      // Build Scala AST
      val scalaDefns = buildScalaAST(spec)
      
      // Build output
      val sb = new StringBuilder
      sb.append(s"// Generated from $specFile\n")
      sb.append(s"package ${spec.name.toLowerCase}\n\n")
      for defn <- scalaDefns do
        sb.append(showScalaDefn(defn))
        sb.append("\n")
      
      // Write to ./tmp
      val outDir = Paths.get("tmp")
      Files.createDirectories(outDir)
      val outFile = outDir.resolve(s"${spec.name}.scala")
      Files.writeString(outFile, sb.toString)
      println(s"Generated: $outFile")

/** Build Scala AST from Phi spec */
def buildScalaAST(spec: LangSpec): List[Val] =
  // Group constructors by return sort
  val ctorsBySortRaw = spec.constructors.groupBy(_.returnSort)
  
  // Determine which sorts can be enums (all constructors nullary)
  val enumSorts = spec.sorts.filter { sort =>
    ctorsBySortRaw.get(sort.name) match
      case Some(ctors) => ctors.nonEmpty && ctors.forall(_.params.isEmpty)
      case None => false
  }.map(_.name).toSet
  
  // Generate enums for simple sorts, sealed traits for complex ones
  val sortDefns = spec.sorts.flatMap { sort =>
    if enumSorts.contains(sort.name) then
      val cases = ctorsBySortRaw(sort.name).map(c => VCon("EnumVal", List(VCon(c.name, Nil))))
      List(VCon("Enum", List(
        VCon(sort.name, Nil),
        VCon("nil", Nil),  // no type params for enums
        VCon("nil", cases)
      )))
    else
      List(sort2Scala(sort))
  }
  
  // Only generate case classes for non-enum constructors
  val ctorDefns = spec.constructors.filterNot(c => enumSorts.contains(c.returnSort)).map(ctor2Scala)
  
  val xformDefns = spec.xforms.flatMap(x => xform2Scala(x, spec.rules))
  
  // Generate interpreter class if there are xforms
  val interpClass = if spec.xforms.nonEmpty then
    List(buildInterpreterClass(spec))
  else Nil
  
  sortDefns ++ ctorDefns ++ xformDefns ++ interpClass

/** Build an interpreter class that wraps the xform methods */
def buildInterpreterClass(spec: LangSpec): Val =
  // Index rules by name
  val rulesByName = spec.rules.groupBy(r => r.name.takeWhile(_ != '.'))
  
  // Generate method wrappers for each xform
  val methods = spec.xforms.flatMap { xform =>
    val forwardRules = spec.rules.filter(_.name.startsWith(xform.name + "."))
    if forwardRules.isEmpty then None
    else
      val srcTy = type2Scala(parsePhiType(xform.source))
      val tgtTy = type2Scala(parsePhiType(xform.target))
      Some(VCon("MethodDef", List(
        VCon(toCamelCase(xform.name), Nil),
        VCon("nil", Nil),
        VCon("nil", List(VCon("Param", List(VCon("input", Nil), srcTy)))),
        VCon("TyApp", List(VCon("SimpleName", List(VCon("Option", Nil))), VCon("nil", List(tgtTy)))),
        VCon("EApp", List(
          VCon("EVar", List(VCon(toCamelCase(xform.name) + "Forward", Nil))),
          VCon("nil", List(VCon("EVar", List(VCon("input", Nil)))))
        ))
      )))
  }
  
  VCon("Class", List(
    VCon(spec.name + "Interpreter", Nil),
    VCon("nil", Nil),  // no type params
    VCon("nil", List(VCon("Param", List(VCon("spec", Nil), VCon("TyName", List(VCon("SimpleName", List(VCon("LangSpec", Nil))))))))),
    VCon("nil", methods)
  ))

/** Sort → SealedTrait(name, typeParams) */
def sort2Scala(sort: Sort): Val =
  VCon("SealedTrait", List(
    VCon(sort.name, Nil),  // String as nullary constructor
    VCon("nil", sort.typeParams.map(p => VCon(p, Nil)))  // List of type params
  ))

/** Infer parameter name from type */
def inferParamName(ty: LangType, index: Int, usedNames: Set[String]): String =
  val base = ty match
    case LangType.SortRef(name) => name.toLowerCase.take(4) match
      case "stri" => "name"
      case "type" => "ty"
      case "pat" | "patt" => "pat"
      case "decl" => "decl"
      case "expr" => "expr"
      case "bind" => "env"
      case "rule" => "rule"
      case "stra" => "strat"
      case "valu" | "val" => "value"
      case "attr" => "attr"
      case _ => name.toLowerCase.take(1)
    case LangType.ListOf(elem) => inferParamName(elem, 0, Set.empty) + "s"
    case LangType.Arrow(_, _) => "f"
    case LangType.Product(_, _) => "pair"
    case _ => s"arg$index"
  
  // Ensure uniqueness
  if !usedNames.contains(base) then base
  else
    var n = 2
    while usedNames.contains(s"$base$n") do n += 1
    s"$base$n"

/** Constructor → CaseClass or CaseObject */
def ctor2Scala(ctor: Constructor): Val =
  var usedNames = Set.empty[String]
  val params = ctor.params.zipWithIndex.map { case ((nameOpt, ty), i) =>
    val argName = nameOpt.getOrElse {
      val inferred = inferParamName(ty, i, usedNames)
      usedNames += inferred
      inferred
    }
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

/** Xform + Rules → MethodDef with match expression */
def xform2Scala(xform: XformSpec, rules: List[Rule]): Option[Val] =
  val forwardRules = rules.filter(_.name.startsWith(xform.name + "."))
  if forwardRules.isEmpty then None
  else
    val srcTy = type2Scala(parsePhiType(xform.source))
    val tgtTy = type2Scala(parsePhiType(xform.target))
    val cases = forwardRules.flatMap(_.cases).map { rcase =>
      VCon("CaseSimple", List(
        pat2ScalaPat(rcase.lhs),
        pat2ScalaExpr(rcase.rhs)
      ))
    }
    Some(VCon("MethodDef", List(
      VCon(toCamelCase(xform.name) + "Forward", Nil),
      VCon("nil", Nil),  // no type params
      VCon("nil", List(VCon("Param", List(VCon("input", Nil), srcTy)))),
      tgtTy,
      VCon("EMatch", List(
        VCon("EVar", List(VCon("input", Nil))),
        VCon("nil", cases)
      ))
    )))

/** MetaPattern → Scala Pattern */
def pat2ScalaPat(p: MetaPattern): Val = p match
  case MetaPattern.PVar(name) => VCon("PVar", List(VCon(name, Nil)))
  // Handle nil as Nil - must come before generic nullary case
  case MetaPattern.PCon("nil", Nil) =>
    VCon("PVar", List(VCon("Nil", Nil)))
  case MetaPattern.PCon(name, Nil) => VCon("PVar", List(VCon(name, Nil)))  // nullary as variable
  // Handle cons as :: infix pattern
  case MetaPattern.PCon("cons", List(h, t)) =>
    VCon("PInfix", List(pat2ScalaPat(h), VCon("::", Nil), pat2ScalaPat(t)))
  // Handle pair as tuple
  case MetaPattern.PCon("pair", List(a, b)) =>
    VCon("PTuple", List(VCon("nil", List(pat2ScalaPat(a), pat2ScalaPat(b)))))
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
  case MetaPattern.PCon("nil", Nil) => VCon("EVar", List(VCon("Nil", Nil)))
  case MetaPattern.PCon(name, Nil) => VCon("EVar", List(VCon(name, Nil)))
  // Handle cons as ::
  case MetaPattern.PCon("cons", List(h, t)) =>
    VCon("EInfix", List(pat2ScalaExpr(h), VCon("::", Nil), pat2ScalaExpr(t)))
  // Handle pair as tuple
  case MetaPattern.PCon("pair", List(a, b)) =>
    VCon("ETuple", List(VCon("nil", List(pat2ScalaExpr(a), pat2ScalaExpr(b)))))
  // Handle xform calls: Parse.forward(x) → parseForward(x)
  case MetaPattern.PCon(name, args) if name.contains(".") =>
    VCon("EApp", List(
      VCon("EVar", List(VCon(toCamelCase(name), Nil))),
      VCon("nil", args.map(pat2ScalaExpr))
    ))
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

// =============================================================================
// Bidirectional Pretty-Printer for Scala AST
// =============================================================================
// These correspond to the syntax declarations in scala.phi (the <-> comments)

def showScalaDefn(v: Val): String = v match
  // SealedTrait(n, tps) <-> "sealed trait " n tps
  case VCon("SealedTrait", List(name, tps)) =>
    val tpStr = showTypeParams(tps)
    s"sealed trait ${showName(name)}$tpStr"
  
  // Enum(n, tps, cases) <-> "enum " n tps " { " cases " }"
  case VCon("Enum", List(name, tps, VCon("nil", cases))) =>
    val tpStr = showTypeParams(tps)
    val casesStr = cases.map(showEnumCase).mkString(", ")
    s"enum ${showName(name)}$tpStr { case $casesStr }"
  
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
  
  // Class(n, tps, ps, members) <-> "class " n tps "(" ps ") {" members "}"
  case VCon("Class", List(name, tps, ps, VCon("nil", members))) =>
    val tpStr = showTypeParams(tps)
    val psStr = showParams(ps)
    val membersStr = members.map(m => "  " + showScalaDefn(m)).mkString("\n")
    s"class ${showName(name)}$tpStr($psStr) {\n$membersStr\n}"
  
  case other => s"/* unknown: ${other.show} */"

def showName(v: Val): String = v match
  case VCon(name, Nil) => name
  case VCon("SimpleName", List(VCon(n, Nil))) => n
  case VCon("QualName", List(base, VCon(n, Nil))) => s"${showName(base)}.$n"
  case other => other.show

def showEnumCase(v: Val): String = v match
  case VCon("EnumVal", List(name)) => showName(name)
  case VCon("EnumClass", List(name, VCon("nil", params))) => 
    s"${showName(name)}(${params.map(showParam).mkString(", ")})"
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
  // Handle cons as ::
  case VCon("EApp", List(VCon("EVar", List(VCon("cons", Nil))), VCon("nil", List(h, t)))) =>
    s"${showExpr(h)} :: ${showExpr(t)}"
  // Handle nil as Nil  
  case VCon("EVar", List(VCon("nil", Nil))) => "Nil"
  // Handle pair as tuple
  case VCon("EApp", List(VCon("EVar", List(VCon("pair", Nil))), VCon("nil", List(a, b)))) =>
    s"(${showExpr(a)}, ${showExpr(b)})"
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
  case VCon("EInfix", List(l, VCon(op, Nil), r)) =>
    s"${showExpr(l)} $op ${showExpr(r)}"
  case VCon("ETuple", List(VCon("nil", elems))) =>
    s"(${elems.map(showExpr).mkString(", ")})"
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
  // Handle cons/nil as :: /Nil
  case VCon("PCon", List(VCon("SimpleName", List(VCon("cons", Nil))), VCon("nil", List(h, t)))) =>
    s"${showPattern(h)} :: ${showPattern(t)}"
  case VCon("PCon", List(VCon("SimpleName", List(VCon("nil", Nil))), VCon("nil", Nil))) =>
    "Nil"
  // Handle pair as tuple
  case VCon("PCon", List(VCon("SimpleName", List(VCon("pair", Nil))), VCon("nil", List(a, b)))) =>
    s"(${showPattern(a)}, ${showPattern(b)})"
  case VCon("PCon", List(name, VCon("nil", args))) => 
    s"${showName(name)}(${args.map(showPattern).mkString(", ")})"
  case VCon("PTuple", List(VCon("nil", elems))) => s"(${elems.map(showPattern).mkString(", ")})"
  case VCon("PInfix", List(l, VCon(op, Nil), r)) => s"${showPattern(l)} $op ${showPattern(r)}"
  case other => other.show
