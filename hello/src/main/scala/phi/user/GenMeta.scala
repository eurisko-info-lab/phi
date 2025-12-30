package phi.user

import java.nio.file.{Files, Paths, Path}
import phi.phi.*
import phi.meta.{Val, LangInterpreter}
import phi.meta.Val.*
import GenBase.*

// GenMeta: Generate gen/*.scala from meta.phi
// 
// Each semantic xform in meta.phi → a generated file in gen/
//   xform Eval(env: Env) : Expr ⇄ Val → gen/Eval.scala (via GenEval xform)
//
// Pipeline:
//   1. Parse meta.phi → LangSpec (semantic definitions)
//   2. Parse meta2scala.phi → LangSpec (Gen* code generators)
//   3. For each semantic xform: apply corresponding Gen* xform → ScalaFile AST
//   4. Render ScalaFile AST via scala.phi grammar → String
//   5. Compare with existing hand-crafted code (if any) and ask user to confirm
//   6. Write to file (if confirmed or no existing file)
object GenMeta:
  
  // Maps semantic xforms to their generators (per-xform)
  val perXformGenerators = Map(
    "Eval"   -> "GenEval",
    "Match"  -> "GenMatch", 
    "Show"   -> "GenShow"
  )
  
  // Generators that work on the full spec (not per-xform)
  val specLevelGenerators = Map(
    "Interp" -> "GenInterp"
  )
  
  // Combined for backwards compat
  val generators = perXformGenerators ++ specLevelGenerators
  
  // Hand-crafted source directory
  val handCraftedDir = Paths.get("src/main/scala/phi/meta/gen")
  
  // Generated output directory  
  val generatedDir = Paths.get("tmp/gen")

  def main(args: Array[String]): Unit =
    parseArgs(args)
    banner("GenMeta: Generating gen/*.scala from meta.phi")
    
    // Load specs
    val metaSpec = loadSpecOrDie("examples/meta.phi", "meta.phi")
    val meta2scalaSpec = loadSpecOrDie("examples/meta2scala.phi", "meta2scala.phi")
    val scalaSpec = loadSpecOrDie("examples/scala.phi", "scala.phi")
    
    val interpreter = LangInterpreter(meta2scalaSpec)
    val renderer = GrammarInterp.specParser(scalaSpec)
    
    // Generate a file for each semantic xform that has a generator
    for 
      xform <- metaSpec.xforms 
      genName <- perXformGenerators.get(xform.name)
    do
      info(s"\nGenerating ${xform.name}.scala via $genName...")
      val xformVal = xformToVal(metaSpec, xform)
      interpreter.applyXform(genName, xformVal) match
        case Some(ast) =>
          if dumpAst then detail(s"AST: ${ast.toString.take(3000)}")
          val code = renderer.render("scalaFile", ast) + "\n"
          val fileName = s"${xform.name}.scala"
          writeWithDiff(code, fileName, handCraftedDir, generatedDir)
        case None =>
          warning(s"$genName rules did not match")
    
    // Generate spec-level files (like Interp which iterates all xforms)
    for (fileName, genName) <- specLevelGenerators do
      info(s"\nGenerating $fileName.scala via $genName...")
      import phi.phi.gen.ToVal.*
      val specVal = metaSpec.toVal
      interpreter.applyXform(genName, specVal) match
        case Some(ast) =>
          if dumpAst then detail(s"AST: ${ast.toString.take(3000)}")
          val code = renderer.render("scalaFile", ast) + "\n"
          writeWithDiff(code, s"$fileName.scala", handCraftedDir, generatedDir)
        case None =>
          warning(s"$genName rules did not match")
    
    info("")
    banner("Generation complete!")
