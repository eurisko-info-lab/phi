package phi.user

import java.nio.file.{Files, Paths}
import phi.phi.*
import phi.meta.Val
import phi.meta.Val.*

// GenMeta: Generate gen/*.scala from meta.phi
// 
// Each xform in meta.phi → a generated file in gen/
//   xform Eval → gen/Eval.scala
//   xform Match → gen/Match.scala  
//   xform Show → gen/Show.scala
//
// Each rule in the xform → an extension method
//   rule Eval.var { EVar(name) ↦ ... } → extension (e: EVar) def evalVar(env: Env)
//
// The pipeline:
//   1. Parse meta.phi → LangSpec
//   2. Build Val AST (ScalaPackage, ScalaObject, ExtensionBlock, ...)
//   3. Render via scala.phi grammar
object GenMeta:

  def main(args: Array[String]): Unit =
    println("=" * 60)
    println("GenMeta: Generating gen/*.scala from meta.phi")
    println("=" * 60)
    
    // 1. Parse meta.phi
    val metaSrc = Files.readString(Paths.get("examples/meta.phi"))
    
    println("\n[1] Parsing meta.phi...")
    val metaSpec = PhiParser.parseSpec(metaSrc) match
      case Right(s) =>
        println(s"    Language: ${s.name}")
        println(s"    Sorts: ${s.sorts.map(_.name).mkString(", ")}")
        println(s"    Constructors: ${s.constructors.map(_.name).mkString(", ")}")
        println(s"    Xforms: ${s.xforms.map(_.name).mkString(", ")}")
        println(s"    Rules: ${s.rules.map(_.name).mkString(", ")}")
        s
      case Left(err) =>
        println(s"    Parse error: $err")
        return
        
    // 2. Parse scala.phi for rendering
    val scalaSrc = Files.readString(Paths.get("examples/scala.phi"))
    
    println("\n[2] Parsing scala.phi...")
    val scalaSpec = PhiParser.parseSpec(scalaSrc) match
      case Right(s) =>
        println(s"    Language: ${s.name}")
        println(s"    Grammars: ${s.grammars.keys.mkString(", ")}")
        s
      case Left(err) =>
        println(s"    Parse error: $err")
        return
    
    val renderer = GrammarInterp.specParser(scalaSpec)
    
    val outputDir = Paths.get("tmp/gen")
    if !Files.exists(outputDir) then Files.createDirectories(outputDir)
    
    // 3. Generate Eval.scala
    println("\n[3] Generating Eval.scala...")
    val evalAst = buildEvalAst(metaSpec)
    val evalCode = renderer.render("scalaFile", evalAst)
    Files.writeString(outputDir.resolve("Eval.scala"), evalCode)
    println(s"    Written to: tmp/gen/Eval.scala")
    
    // 4. Generate Match.scala
    println("\n[4] Generating Match.scala...")
    val matchAst = buildMatchAst(metaSpec)
    val matchCode = renderer.render("scalaFile", matchAst)
    Files.writeString(outputDir.resolve("Match.scala"), matchCode)
    println(s"    Written to: tmp/gen/Match.scala")
    
    // 5. Generate Show.scala
    println("\n[5] Generating Show.scala...")
    val showAst = buildShowAst(metaSpec)
    val showCode = renderer.render("scalaFile", showAst)
    Files.writeString(outputDir.resolve("Show.scala"), showCode)
    println(s"    Written to: tmp/gen/Show.scala")
    
    println("\n" + "=" * 60)
    println("Generation complete!")
    println("=" * 60)

  // ==========================================================================
  // AST Builders - construct Val representing Scala code
  // Uses constructors from scala.phi
  // ==========================================================================
  
  def buildEvalAst(spec: LangSpec): Val =
    val exprCons = spec.constructors.filter(_.returnType == "Expr")
    
    // ScalaPackage("phi.meta.gen", [ScalaObject("Eval", [extensions...])])
    VCon("ScalaPackage", List(
      VStr("phi.meta.gen"),
      VList(List(
        VCon("ScalaObject", List(
          VStr("Eval"),
          VList(
            exprCons.map { c =>
              val methodName = "eval" + c.name.stripPrefix("E")
              // extension (e: EVar) ...
              VCon("ExtensionOn", List(
                VStr("e"),
                VStr(c.name),
                VList(Nil)  // Empty body for now
              ))
            }
          )
        ))
      ))
    ))

  def buildMatchAst(spec: LangSpec): Val =
    val patCons = spec.constructors.filter(_.returnType == "Pat")
    
    VCon("ScalaPackage", List(
      VStr("phi.meta.gen"),
      VList(List(
        VCon("ScalaObject", List(
          VStr("Match"),
          VList(
            patCons.map { c =>
              VCon("ExtensionOn", List(
                VStr("p"),
                VStr(c.name),
                VList(Nil)
              ))
            }
          )
        ))
      ))
    ))

  def buildShowAst(spec: LangSpec): Val =
    val grammars = spec.grammars.keys.toList
    
    VCon("ScalaPackage", List(
      VStr("phi.meta.gen"),
      VList(List(
        VCon("ScalaObject", List(
          VStr("Show"),
          VList(Nil)  // Empty for now - Show derived from grammars
        ))
      ))
    ))
