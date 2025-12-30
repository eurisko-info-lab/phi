package phi.user

import java.nio.file.{Files, Paths}
import phi.phi.*
import phi.meta.{Val, LangInterpreter}
import phi.meta.Val.*

// GenMeta: Generate gen/*.scala from meta.phi
// 
// Each semantic xform in meta.phi → a generated file in gen/
//   xform Eval(env: Env) : Expr ⇄ Val → gen/Eval.scala (via GenEval xform)
//
// Pipeline:
//   1. Parse meta.phi → LangSpec (includes Gen* xforms)
//   2. For each semantic xform: apply corresponding Gen* xform → ScalaFile AST
//   3. Render ScalaFile AST via scala.phi grammar → String
//   4. Write to file
//
// GenMeta knows nothing about Scala - it just orchestrates xform calls.
// All Scala-specific knowledge is in the Gen* rules in meta.phi.
object GenMeta:
  
  // Maps semantic xforms to their generators
  val generators = Map(
    "Eval"  -> "GenEval",
    "Match" -> "GenMatch", 
    "Show"  -> "GenShow",
    "Subst" -> "GenSubst"
  )

  def main(args: Array[String]): Unit =
    println("=" * 60)
    println("GenMeta: Generating gen/*.scala from meta.phi")
    println("=" * 60)
    
    val metaSrc = Files.readString(Paths.get("examples/meta.phi"))
    val metaSpec = PhiParser.parseSpec(metaSrc) match
      case Right(s) => s
      case Left(err) => println(s"Parse error: $err"); return
        
    val scalaSrc = Files.readString(Paths.get("examples/scala.phi"))
    val scalaSpec = PhiParser.parseSpec(scalaSrc) match
      case Right(s) => s
      case Left(err) => println(s"Parse error: $err"); return
    
    val interpreter = LangInterpreter(metaSpec)
    val renderer = GrammarInterp.specParser(scalaSpec)
    val outputDir = Paths.get("tmp/gen")
    if !Files.exists(outputDir) then Files.createDirectories(outputDir)
    
    // Generate a file for each semantic xform that has a generator
    for 
      xform <- metaSpec.xforms 
      genName <- generators.get(xform.name)
    do
      println(s"\nGenerating ${xform.name}.scala via $genName...")
      val xformVal = xformToVal(metaSpec, xform)
      interpreter.applyXform(genName, xformVal) match
        case Some(ast) =>
          val code = renderer.render("scalaFile", ast)
          Files.writeString(outputDir.resolve(s"${xform.name}.scala"), code)
          println(s"    Written to: tmp/gen/${xform.name}.scala")
        case None =>
          println(s"    Warning: $genName rules did not match")
    
    println("\n" + "=" * 60)
    println("Generation complete!")
    println("=" * 60)

  /** Convert Xform case class to Val representation */
  def xformToVal(spec: LangSpec, xform: Xform): Val =
    val rules = spec.rules.filter(_.name.startsWith(s"${xform.name}."))
    VCon("Xform", List(
      VStr(xform.name),
      toCons(xform.params.map { case (n, t) => VCon("Param", List(VStr(n), VStr(t))) }),
      VStr(xform.srcType),
      VStr(xform.tgtType),
      toCons(rules.map(ruleToVal))
    ))
  
  /** Convert Scala List to Cons/Nil Val representation */
  def toCons(list: List[Val]): Val = list match
    case Nil => VCon("Nil", Nil)
    case head :: tail => VCon("Cons", List(head, toCons(tail)))
  
  /** Convert Rule to Val */
  def ruleToVal(rule: Rule): Val =
    VCon("Rule", List(
      VStr(rule.name),
      toCons(rule.cases.map(c => VCon("RuleCase", List(patternToVal(c.pattern), patternToVal(c.body)))))
    ))
  
  def patternToVal(p: MetaPattern): Val = p match
    case MetaPattern.PVar(n) => VCon("PVar", List(VStr(n)))
    case MetaPattern.PCon(n, args) => VCon("PCon", List(VStr(n), toCons(args.map(patternToVal))))
    case MetaPattern.PApp(f, a) => VCon("PApp", List(patternToVal(f), patternToVal(a)))
    case MetaPattern.PSubst(b, v, r) => VCon("PSubst", List(patternToVal(b), VStr(v), patternToVal(r)))
