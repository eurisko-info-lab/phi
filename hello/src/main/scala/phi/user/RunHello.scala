package phi.user

import java.nio.file.{Files, Paths}
import phi.meta.*
import phi.meta.Val.*
import phi.meta.gen.Show.show
import phi.phi.*

/**
 * RunHello: Interpreted version - uses specs at runtime
 * 
 * Unlike GenHello (which has hardcoded rendering), this version:
 * 1. Loads hello.phi, scala.phi, hello2scala.phi at runtime
 * 2. Parses input using hello.phi grammar
 * 3. Transforms using hello2scala.phi rules
 * 4. Renders using scala.phi grammar (bidirectional!)
 * 
 * This demonstrates the full interpreted pipeline.
 */
object RunHello:

  def main(args: Array[String]): Unit =
    println("=" * 60)
    println("RunHello: Interpreted Pipeline Demo")
    println("=" * 60)
    
    // 1. Load all specs
    println("\n[1] Loading specs...")
    val helloSpec = loadSpec("examples/hello.phi")
    val scalaSpec = loadSpec("examples/scala.phi")
    val xformSpec = loadSpec("examples/hello2scala.phi")
    
    println(s"    ✓ hello.phi: ${helloSpec.name}")
    println(s"    ✓ scala.phi: ${scalaSpec.name}")
    println(s"    ✓ hello2scala.phi: ${xformSpec.name}")
    
    // 2. Parse input using hello.phi grammar
    val input = args.headOption.getOrElse("Hello World")
    println(s"\n[2] Parsing: \"$input\"")
    
    val parser = GrammarInterp.specParser(helloSpec)
    val ast = parser.parse("greeting", input) match
      case Right(v) =>
        println(s"    ✓ AST: ${v.show}")
        v
      case Left(err) =>
        println(s"    ✗ Parse error: $err")
        return
    
    // 3. Transform using hello2scala.phi
    println("\n[3] Transforming with hello2scala...")
    val interp = LangInterpreter(xformSpec)
    val scalaAst = interp.applyXform("Greeting2Scala", ast) match
      case Some(v) =>
        println(s"    ✓ Scala AST: ${v.show}")
        v
      case None =>
        println("    ✗ Transform failed")
        return
    
    // 4. Render using scala.phi grammar (interpreted!)
    println("\n[4] Rendering with scala.phi grammar...")
    val renderer = GrammarRenderer(scalaSpec)
    val scalaCode = renderer.render("scalaFile", scalaAst)
    println(s"    ✓ Output:\n$scalaCode")
    
    // 5. Also show the fallback render for comparison
    println("\n[5] Fallback render (structural):")
    val fallback = structuralRender(scalaAst)
    println(s"    $fallback")
    
    println("\n" + "=" * 60)
    println("Interpreted pipeline complete!")
    println("=" * 60)

  def loadSpec(path: String): LangSpec =
    val src = Files.readString(Paths.get(path))
    PhiParser.parseSpec(src) match
      case Right(spec) => spec
      case Left(err) => throw RuntimeException(s"Failed to parse $path: $err")

  /** Structural render - walks the AST directly */
  def structuralRender(v: Val): String = v match
    case VCon("ScalaObject", List(name, body)) =>
      s"object ${extractStr(name)}:\n${indent(structuralRender(body))}"
    case VCon("MainDef", List(stmt)) =>
      s"def main(args: Array[String]): Unit =\n${indent(structuralRender(stmt))}"
    case VCon("PrintLn", List(expr)) =>
      s"println(${structuralRender(expr)})"
    case VCon("StringLit", List(s)) =>
      s"\"${extractStr(s)}\""
    case VCon("StringConcat", List(l, r)) =>
      s"${structuralRender(l)} + ${structuralRender(r)}"
    case VCon(name, Nil) => name
    case VCon(name, args) => s"$name(${args.map(structuralRender).mkString(", ")})"
    case VStr(s) => s"\"$s\""
    case VInt(n) => n.toString
    case VList(elems) => s"List(${elems.map(structuralRender).mkString(", ")})"

  def extractStr(v: Val): String = v match
    case VCon("String", List(VCon(s, Nil))) => s
    case VCon(s, Nil) => s
    case VStr(s) => s
    case _ => v.show
  
  def indent(s: String): String = s.linesIterator.map("  " + _).mkString("\n")

/**
 * GrammarRenderer: Render values using grammar rules (bidirectional!)
 * 
 * This is the inverse of parsing - given a Val and a grammar,
 * produce the string representation.
 */
class GrammarRenderer(spec: LangSpec):
  
  def render(grammarName: String, value: Val): String =
    spec.grammars.get(grammarName) match
      case None => s"/* unknown grammar: $grammarName */ ${value.show}"
      case Some(rules) =>
        tryRules(rules, value).getOrElse(s"/* no rule matched */ ${value.show}")
  
  private def tryRules(rules: List[SyntaxRule], value: Val): Option[String] =
    rules.iterator.flatMap(tryRule(_, value)).nextOption()
  
  private def tryRule(rule: SyntaxRule, value: Val): Option[String] =
    // Match the result pattern against the value to get bindings
    matchResult(rule.result, value).map { bindings =>
      // Render the tokens using the bindings
      renderTokens(rule.pattern, bindings)
    }
  
  private def matchResult(arg: SyntaxArg, value: Val): Option[Map[String, Val]] =
    (arg, value) match
      case (SyntaxArg.Con(name, argArgs), VCon(vname, vargs)) if name == vname =>
        if argArgs.length == vargs.length then
          argArgs.zip(vargs).foldLeft[Option[Map[String, Val]]](Some(Map.empty)) {
            case (Some(acc), (a, v)) =>
              matchResult(a, v).map(acc ++ _)
            case (None, _) => None
          }
        else None
      case (SyntaxArg.Ref(name), v) =>
        Some(Map(name -> v))
      case _ => None
  
  private def renderTokens(tokens: List[SyntaxToken], bindings: Map[String, Val]): String =
    tokens.map {
      case SyntaxToken.Literal(text) => text
      case SyntaxToken.NonTerm(name, _) =>
        bindings.get(name) match
          case Some(v) => renderValue(name, v)
          case None => s"?$name"
    }.mkString(" ")
  
  private def renderValue(grammarName: String, value: Val): String =
    // Try to find a grammar for this name
    spec.grammars.get(grammarName) match
      case Some(rules) => tryRules(rules, value).getOrElse(value.show)
      case None =>
        // Fallback: render the value directly
        value match
          case VCon(name, Nil) => name
          case VCon(name, args) => s"$name(${args.map(_.show).mkString(", ")})"
          case VStr(s) => s"\"$s\""
          case VInt(n) => n.toString
          case VList(elems) => elems.map(_.show).mkString(", ")
