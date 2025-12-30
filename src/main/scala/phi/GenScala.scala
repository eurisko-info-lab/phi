package phi

import java.nio.file.{Files, Paths, Path}
import Val.*
import scala.collection.mutable

// =============================================================================
// GenScala: Generate Scala code from Phi language specs
// =============================================================================
//
// This is a bootstrapping code generator. It:
// 1. Parses a .phi spec file
// 2. Generates Scala code (case classes, sealed traits, enums)
// 3. Generates parser combinators from grammar blocks
// 4. Generates interpreter methods from xform rules
//
// Future: Replace hardcoded transforms with MetaInterpreter execution of phi2scala.phi rules.

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

/** Convert a cons-list Val to a Scala List[Val] 
  * Handles: nil, cons(head, tail), concat(xs, ys)
  */
def consListToScalaList(v: Val): List[Val] = v match
  case VCon("nil", Nil) => Nil
  case VCon("cons", List(head, tail)) => head :: consListToScalaList(tail)
  case VCon("concat", List(xs, ys)) => consListToScalaList(xs) ++ consListToScalaList(ys)
  case other => List(other)  // Single item fallback

@main def GenScala(specFile: String): Unit =
  // Load phi2scala transform spec
  val phi2scalaSpec = parseSpecWithInheritance("examples/phi2scala.phi") match
    case Right(spec) => spec
    case Left(err) => throw new RuntimeException(s"Failed to load phi2scala.phi: $err")
  
  println(s"Loaded phi2scala with ${phi2scalaSpec.rules.length} rules: ${phi2scalaSpec.rules.map(_.name).mkString(", ")}")
  
  val phi2scala = new LangInterpreter(phi2scalaSpec)
  
  parseSpecWithInheritance(specFile) match
    case Left(err) =>
      System.err.println(s"Parse error: $err")
      sys.exit(1)
    case Right(spec) =>
      // Load scala.phi grammar for printing
      val scalaSpec = parseSpecWithInheritance("examples/scala.phi") match
        case Right(spec) => spec
        case Left(err) => throw new RuntimeException(s"Failed to load scala.phi: $err")
      
      val printer = GrammarInterp.specParser(scalaSpec)
      
      // Transform spec to Scala AST via phi2scala rules
      // Rules are named "Spec2Scala.spec", so look for the base name prefix
      val specCases = phi2scalaSpec.rules.filter(_.name.startsWith("Spec2Scala.")).flatMap(_.cases)
      println(s"Spec2Scala cases: ${specCases.length}")
      val result = phi2scala.applyRule(spec.toVal, specCases)
      println(s"Transform result: ${result.map(v => v.show.take(500))}")
      
      val scalaDefns = result match
        case Some(VCon("SourceFile", List(_, _, defns))) => 
          // defns is a cons-list
          println(s"defns: ${defns.show.take(500)}")
          consListToScalaList(defns)
        case Some(other) => List(other)
        case None => Nil
      
      println(s"scalaDefns count: ${scalaDefns.length}")
      // Print first few defns to debug
      val ctors = scalaDefns.collect { case VCon(name, _) => name }.distinct
      println(s"Constructors: ${ctors.mkString(", ")}")
      for defn <- scalaDefns.take(10) do
        val VCon(name, args) = defn: @unchecked
        println(s"  defn: ${defn.show}")
        val rendered = printer.render("defn", defn)
        println(s"    rendered: '$rendered'")
      
      // Build output
      val sb = new StringBuilder
      sb.append(s"// Generated from $specFile\n")
      sb.append(s"package ${spec.name.toLowerCase}\n\n")
      
      // Imports if needed
      if spec.xforms.nonEmpty || spec.rules.nonEmpty then
        sb.append("import scala.collection.mutable\n\n")
      
      // Print via scala.phi grammar (backwards)
      for defn <- scalaDefns do
        sb.append(printer.render("defn", defn))
        sb.append("\n\n")
      
      // Generate prelude functions (concat, map, etc.) if xforms present
      if spec.xforms.nonEmpty then
        sb.append(generatePrelude())
        sb.append("\n")
      
      // Write to ./tmp
      val outDir = Paths.get("tmp")
      Files.createDirectories(outDir)
      val outFile = outDir.resolve(s"${spec.name}.scala")
      Files.writeString(outFile, sb.toString)
      println(s"Generated: $outFile")
      
      // Also generate parser if spec has grammar rules
      if spec.grammars.nonEmpty then
        val parserCode = generateParser(spec)
        val parserFile = outDir.resolve(s"${spec.name}Parser.scala")
        Files.writeString(parserFile, parserCode)
        println(s"Generated: $parserFile")

/** Generate prelude functions used by generated transforms */
def generatePrelude(): String =
  """
// =============================================================================
// Prelude: Helper functions for generated transforms
// =============================================================================

def concat[A](xs: List[A], ys: List[A]): List[A] = xs ++ ys
def cons[A](x: A, xs: List[A]): List[A] = x :: xs
def nil[A]: List[A] = Nil
def pair[A, B](a: A, b: B): (A, B) = (a, b)
def some[A](a: A): Option[A] = Some(a)
def none[A]: Option[A] = None
def fst[A, B](p: (A, B)): A = p._1
def snd[A, B](p: (A, B)): B = p._2
def map[A, B](f: A => B, xs: List[A]): List[B] = xs.map(f)
def flatMap[A, B](f: A => List[B], xs: List[A]): List[B] = xs.flatMap(f)
def filter[A](p: A => Boolean, xs: List[A]): List[A] = xs.filter(p)
def foldLeft[A, B](f: (B, A) => B, z: B, xs: List[A]): B = xs.foldLeft(z)(f)
def foldRight[A, B](f: (A, B) => B, z: B, xs: List[A]): B = xs.foldRight(z)(f)
"""

// =============================================================================
// Parser Generation from Grammar Rules
// =============================================================================

/** Generate parser combinators from a Phi spec's grammar rules */
def generateParser(spec: LangSpec): String =
  val sb = new StringBuilder
  sb.append(s"// Generated parser for ${spec.name}\n")
  sb.append("package phi\n\n")
  sb.append("import scala.util.parsing.combinator.*\n\n")
  sb.append(s"object ${spec.name}Parser extends RegexParsers:\n")
  sb.append("  override val whiteSpace = \"\"\"(\\s|//[^\\n]*)+\"\"\".r\n")
  sb.append("  def ident: Parser[String] = \"\"\"[a-zA-Z_][a-zA-Z0-9_]*\"\"\".r\n")
  sb.append("  def number: Parser[Int] = \"\"\"\\d+\"\"\".r ^^ (_.toInt)\n")
  sb.append("  def stringLit: Parser[String] = \"\"\"\"[^\"]*\"\"\"\".r ^^ { s => s.substring(1, s.length - 1) }\n\n")
  
  // Generate parser methods for each grammar
  for (grammarName, rules) <- spec.grammars do
    sb.append(generateGrammarParser(grammarName, rules))
    sb.append("\n")
  
  sb.toString

/** Generate a parser method for a grammar */
def generateGrammarParser(name: String, rules: List[SyntaxRule]): String =
  val sb = new StringBuilder
  // Find the return type from constructor names
  val returnType = rules.headOption.map(_.constructor).getOrElse("Any")
  val escapedName = escapeKeyword(name)
  sb.append(s"  def $escapedName: Parser[$returnType] =\n")
  
  if rules.isEmpty then
    sb.append(s"    failure(\"no rules for $name\")\n")
  else
    val ruleStrs = rules.map(rule => generateRuleParser(rule))
    sb.append(ruleStrs.mkString(" |\n"))
    sb.append("\n")
  
  sb.toString

/** Generate parser for a single grammar rule */
def generateRuleParser(rule: SyntaxRule): String =
  // rule.tokens is the LHS (what to match)
  // rule.constructor is the constructor name
  // rule.args is how to build the result
  
  val tokenParsers = rule.tokens.map(tokenToParser)
  val combined = tokenParsers.mkString(" ~ ")
  
  // Build the pattern and result
  val (pattern, result) = buildPatternAndResult(rule)
  
  if pattern.isEmpty then
    s"    ($combined) ^^ { _ => $result }"
  else
    s"    ($combined) ^^ { case $pattern => $result }"

/** Convert a syntax token to a parser expression */
def tokenToParser(token: SyntaxToken): String = token match
  case SyntaxToken.Literal(s) => 
    val escaped = s.replace("\"", "\\\"")
    s"\"$escaped\""
  case SyntaxToken.NonTerm(name, None) => escapeKeyword(name)
  case SyntaxToken.NonTerm(name, Some("*")) => s"rep(${escapeKeyword(name)})"
  case SyntaxToken.NonTerm(name, Some("+")) => s"rep1(${escapeKeyword(name)})"
  case SyntaxToken.NonTerm(name, Some("?")) => s"opt(${escapeKeyword(name)})"
  case SyntaxToken.NonTerm(name, Some(m)) => s"${escapeKeyword(name)} /* modifier: $m */"

/** Escape name if it's a Scala keyword */
def escapeKeyword(name: String): String = name match
  case "type" => "typeExpr"
  case "val" => "valExpr"
  case "var" => "varExpr"
  case "def" => "defExpr"
  case "class" => "classExpr"
  case "object" => "objectExpr"
  case "trait" => "traitExpr"
  case "match" => "matchExpr"
  case "case" => "caseExpr"
  case "if" => "ifExpr"
  case "else" => "elseExpr"
  case "for" => "forExpr"
  case "while" => "whileExpr"
  case "import" => "importDecl"
  case _ => name

/** Build the case pattern and result expression for a rule */
def buildPatternAndResult(rule: SyntaxRule): (String, String) =
  // Build pattern that matches all tokens, using ~ for combination
  // Variables for non-terminals, _ for literals
  val patternParts = rule.tokens.zipWithIndex.map { case (tok, i) =>
    tok match
      case SyntaxToken.Literal(_) => "_"
      case SyntaxToken.NonTerm(name, _) => s"v$i"
  }
  val pattern = if patternParts.isEmpty then "" else patternParts.mkString(" ~ ")
  
  // Map non-terminal names to their variable indices
  val nameToVar = rule.tokens.zipWithIndex.collect {
    case (SyntaxToken.NonTerm(name, _), i) => (name, s"v$i")
  }.toMap
  
  // Get list of non-terminal vars in order
  val nonTermVars = rule.tokens.zipWithIndex.collect {
    case (SyntaxToken.NonTerm(_, _), i) => s"v$i"
  }
  
  // Build constructor call
  val args = rule.args match
    case Nil =>
      // No explicit args - use non-terminals in order
      nonTermVars.mkString(", ")
    case _ =>
      // Use args to map positions, with holes using positional vars
      var holeIdx = 0
      rule.args.map { arg =>
        arg match
          case SyntaxArg.Hole =>
            val result = if holeIdx < nonTermVars.length then nonTermVars(holeIdx) else "_"
            holeIdx += 1
            result
          case other => argToScala(other, nameToVar)
      }.mkString(", ")
  
  val result = s"${rule.constructor}($args)"
  (pattern, result)

def argToScala(arg: SyntaxArg, nameToVar: Map[String, String]): String = arg match
  case SyntaxArg.Hole => "_"
  case SyntaxArg.Ref(name) => nameToVar.getOrElse(name, name)
  case SyntaxArg.Lit(value) => s"\"$value\""
  case SyntaxArg.StrLit(value) => s"\"$value\""
  case SyntaxArg.Wrap(wrapper, inner) => s"$wrapper(${argToScala(inner, nameToVar)})"
  case SyntaxArg.Con(name, args) => 
    s"$name(${args.map(a => argToScala(a, nameToVar)).mkString(", ")})"

