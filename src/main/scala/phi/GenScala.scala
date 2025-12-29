package phi

import java.nio.file.{Files, Paths}

/**
 * Generate Scala code from Phi language specs.
 * 
 * Usage: sbt "runMain phi.GenScala examples/stlc-nat.phi"
 * 
 * Converts Phi sorts → sealed traits, constructors → case classes
 */
@main def GenScala(specFile: String): Unit =
  println(s"// Generated from $specFile")
  println()
  
  parseSpecWithInheritance(specFile) match
    case Left(err) =>
      System.err.println(s"Parse error: $err")
      sys.exit(1)
    case Right(spec) =>
      // Package declaration
      println(s"package ${spec.name.toLowerCase}")
      println()
      
      // Generate sealed traits for sorts
      for sort <- spec.sorts do
        if sort.typeParams.isEmpty then
          println(s"sealed trait ${sort.name}")
        else
          println(s"sealed trait ${sort.name}[${sort.typeParams.mkString(", ")}]")
      
      println()
      
      // Generate case classes for constructors
      for ctor <- spec.constructors do
        val paramStr = ctor.params.zipWithIndex.map { case ((nameOpt, ty), i) =>
          val argName = nameOpt.getOrElse(s"arg$i")
          s"$argName: ${formatType(ty)}"
        }.mkString(", ")
        
        val extendsClause = s" extends ${ctor.returnSort}"
        
        if ctor.params.isEmpty then
          println(s"case object ${ctor.name}$extendsClause")
        else
          println(s"case class ${ctor.name}($paramStr)$extendsClause")
      
      println()
      
      // Generate xforms as method signatures
      for xform <- spec.xforms do
        println(s"// xform ${xform.name} : ${xform.source} ⇄ ${xform.target}")
        
        // Find rules for this xform
        val forwardRules = spec.rules.filter(_.name.startsWith(xform.name + "."))
        if forwardRules.nonEmpty then
          println(s"def ${toCamelCase(xform.name)}Forward(input: ${xform.source}): ${xform.target} = input match {")
          for rule <- forwardRules do
            for rcase <- rule.cases do
              val pat = formatPattern(rcase.lhs)
              val body = formatExpr(rcase.rhs)
              println(s"  case $pat => $body")
          println("}")
        println()

def toCamelCase(s: String): String =
  val parts = s.split("[._]")
  parts.head.toLowerCase + parts.tail.map(_.capitalize).mkString

/** Format a LangType as Scala type */
def formatType(ty: LangType): String = ty match
  case LangType.SortRef(name) => name
  case LangType.TypeApp(base, args) => s"$base[${args.map(formatType).mkString(", ")}]"
  case LangType.TypeVar(name) => name
  case LangType.Arrow(from, to) => s"${formatType(from)} => ${formatType(to)}"
  case LangType.Product(a, b) => s"(${formatType(a)}, ${formatType(b)})"
  case LangType.ListOf(elem) => s"List[${formatType(elem)}]"

/** Format a MetaPattern as Scala pattern */
def formatPattern(p: MetaPattern): String = p match
  case MetaPattern.PVar(name) => name
  case MetaPattern.PCon(name, Nil) => name
  case MetaPattern.PCon(name, args) => s"$name(${args.map(formatPattern).mkString(", ")})"
  case MetaPattern.PApp(f, a) => s"${formatPattern(f)}(${formatPattern(a)})"
  case MetaPattern.PSubst(body, v, repl) => s"${formatPattern(body)}[$v := ${formatPattern(repl)}]"

/** Format a MetaPattern as Scala expression */
def formatExpr(p: MetaPattern): String = p match
  case MetaPattern.PVar(name) => name
  case MetaPattern.PCon(name, Nil) => name
  case MetaPattern.PCon(name, args) => s"$name(${args.map(formatExpr).mkString(", ")})"
  case MetaPattern.PApp(f, a) => s"${formatExpr(f)}(${formatExpr(a)})"
  case MetaPattern.PSubst(body, v, repl) => 
    // Substitution becomes a method call
    s"subst(${formatExpr(body)}, \"$v\", ${formatExpr(repl)})"
