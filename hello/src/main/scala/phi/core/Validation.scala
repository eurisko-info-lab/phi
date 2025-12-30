package phi.core

import phi.meta.{Term, Syntax, ParseResult, TokenStream, Lexer, Lex, Renderer}
import phi.phi.{LangSpec, GrammarParser, GrammarInterp}
import phi.meta.Val

/**
 * Validation utilities for round-trip correctness.
 * 
 * Makes explicit the validation that should be done at every step:
 * - Parse/render round-trip
 * - Xform forward/backward round-trip
 * - Grammar interpretation round-trip
 */
object Validation:
  
  /** Result of a validation check */
  enum ValidationResult:
    case Pass(msg: String)
    case Fail(msg: String, details: Option[String] = None)
    
    def isPass: Boolean = this match
      case Pass(_) => true
      case Fail(_, _) => false
    
    def message: String = this match
      case Pass(m) => s"✓ $m"
      case Fail(m, d) => s"✗ $m" + d.map(s => s"\n  $s").getOrElse("")

  // ===========================================================================
  // Syntax Round-Trip
  // ===========================================================================
  
  /** Check that parse(render(parse(input))) == parse(input) */
  def syntaxRoundTrip[A](syntax: Syntax[A], input: String): ValidationResult =
    val stream = Lexer.tokenize(input)
    val parsed = syntax.parse(stream)
    parsed.term match
      case Term.Done(value) =>
        val rendered = Renderer.render(syntax, Term.Done(value))
        val stream2 = Lexer.tokenize(rendered)
        val reparsed = syntax.parse(stream2)
        reparsed.term match
          case Term.Done(value2) if value == value2 =>
            ValidationResult.Pass(s"Syntax round-trip: '$input' → '$rendered' → same")
          case Term.Done(value2) =>
            ValidationResult.Fail(
              s"Syntax round-trip mismatch",
              Some(s"Original: $value\nReparsed: $value2\nRendered: '$rendered'")
            )
          case Term.Hole(l) =>
            ValidationResult.Fail(s"Reparse failed", l)
      case Term.Hole(l) =>
        ValidationResult.Fail(s"Initial parse failed", l)

  // ===========================================================================
  // Xform Round-Trip
  // ===========================================================================
  
  /** Check that backward(forward(term)) == term */
  def xformRoundTrip[A, B](xform: Xform[A, B], term: Term[A]): ValidationResult =
    val forward = xform.forward(term)
    forward match
      case Term.Hole(l) =>
        ValidationResult.Fail(s"Forward transform failed", l)
      case Term.Done(_) =>
        val back = xform.backward(forward)
        back match
          case Term.Hole(l) =>
            ValidationResult.Fail(s"Backward transform failed", l)
          case Term.Done(_) if back == term =>
            ValidationResult.Pass(s"Xform round-trip preserved")
          case _ =>
            ValidationResult.Fail(
              s"Xform round-trip mismatch",
              Some(s"Original: $term\nBack: $back")
            )

  // ===========================================================================
  // Grammar Round-Trip
  // ===========================================================================
  
  /** Check that render(parse(input)) parses back to the same value */
  def grammarRoundTrip(spec: LangSpec, grammarName: String, input: String): ValidationResult =
    val parser = GrammarInterp.specParser(spec)
    parser.parse(grammarName, input) match
      case Left(err) =>
        ValidationResult.Fail(s"Parse failed", Some(err))
      case Right(value) =>
        val rendered = parser.render(grammarName, value)
        parser.parse(grammarName, rendered) match
          case Left(err) =>
            ValidationResult.Fail(s"Reparse failed", Some(s"Rendered: '$rendered'\nError: $err"))
          case Right(value2) if value == value2 =>
            ValidationResult.Pass(s"Grammar round-trip: '$input' → '$rendered' → same")
          case Right(value2) =>
            ValidationResult.Fail(
              s"Grammar round-trip mismatch",
              Some(s"Original: $value\nReparsed: $value2\nRendered: '$rendered'")
            )

  // ===========================================================================
  // Spec Completeness
  // ===========================================================================
  
  /** Check that all constructors have grammar rules */
  def grammarCompleteness(spec: LangSpec, grammarName: String): ValidationResult =
    spec.grammars.get(grammarName) match
      case None =>
        ValidationResult.Fail(s"Grammar '$grammarName' not found")
      case Some(rules) =>
        val constructorNames = spec.constructors.map(_.name).toSet
        val coveredByRules = rules.flatMap { rule =>
          rule.result match
            case phi.phi.SyntaxArg.Con(name, _) => Some(name)
            case _ => None
        }.toSet
        
        val returnType = spec.constructors
          .filter(c => coveredByRules.contains(c.name))
          .map(_.returnType)
          .headOption
        
        val relevantConstructors = spec.constructors
          .filter(c => returnType.contains(c.returnType))
          .map(_.name)
          .toSet
        
        val missing = relevantConstructors -- coveredByRules
        if missing.isEmpty then
          ValidationResult.Pass(s"Grammar '$grammarName' covers all constructors")
        else
          ValidationResult.Fail(
            s"Grammar '$grammarName' missing constructors",
            Some(missing.mkString(", "))
          )

  // ===========================================================================
  // Batch Validation
  // ===========================================================================
  
  /** Run multiple validations and collect results */
  def validateAll(checks: List[() => ValidationResult]): List[ValidationResult] =
    checks.map(_())

  /** Print validation results */
  def report(results: List[ValidationResult]): Unit =
    val (passed, failed) = results.partition(_.isPass)
    println(s"\n=== Validation Report ===")
    println(s"Passed: ${passed.length}/${results.length}")
    results.foreach(r => println(r.message))
    if failed.nonEmpty then
      println(s"\n${failed.length} check(s) failed")
