package phi.user

import java.nio.file.{Path, Paths}
import phi.phi.*
import phi.phi.gen.ToVal.*
import phi.meta.LangInterpreter
import GenBase.*

// GenPhi: Generate phi.phi.gen/*.scala from phi.phi
// 
// Generates from the LangSpec itself (types/constructors):
//   - GenRender generates gen/Render.scala (pretty printing for LangSpec types)
//   - GenToVal generates gen/ToVal.scala (converting types to Val)
//   - GenFold generates gen/Fold.scala (generic fold/visitor trait)
//   - GenValidate generates gen/Validate.scala (semantic validation)
//
// Pipeline:
//   1. Parse phi.phi → LangSpec (language spec definitions)
//   2. Parse phi2scala.phi → LangSpec (Gen* code generators)
//   3. Apply Gen* xforms → ScalaFile AST
//   4. Render ScalaFile AST via scala.phi grammar → String
//   5. Compare with existing code and write if needed
object GenPhi:
  
  // Maps output files to their generators
  val generators: Map[String, String] = Map(
    "Render"   -> "GenRender",
    "ToVal"    -> "GenToVal",
    "Fold"     -> "GenFold",
    "Validate" -> "GenValidate",
    "Parser"   -> "GenParser"
  )
  
  // Hand-crafted source directory
  val handCraftedDir: Path = Paths.get("src/main/scala/phi/phi/gen")
  
  // Generated output directory  
  val generatedDir: Path = Paths.get("tmp/phi-gen")

  def main(args: Array[String]): Unit =
    parseArgs(args)
    banner("GenPhi: Generating phi.phi.gen/*.scala from phi.phi")
    
    // Load specs
    val phiSpec = loadSpecOrDie("examples/phi.phi", "phi.phi")
    val phi2scalaSpec = loadSpecOrDie("examples/phi2scala.phi", "phi2scala.phi")
    val scalaSpec = loadSpecOrDie("examples/scala.phi", "scala.phi")
    
    val interpreter = LangInterpreter(phi2scalaSpec)
    val renderer = GrammarInterp.specParser(scalaSpec)
    
    // Generate from spec itself - use generated toVal!
    for genName <- generators.values do
      info(s"\nGenerating via $genName...")
      val specVal = phiSpec.toVal  // Using generated extension method!
      
      interpreter.applyXform(genName, specVal) match
        case Some(ast) =>
          if dumpAst then detail(s"AST: ${ast.toString.take(2000)}")
          val code = renderer.render("scalaFile", ast) + "\n"
          val fileName = genName.stripPrefix("Gen") + ".scala"
          writeWithDiff(code, fileName, handCraftedDir, generatedDir)
        case None =>
          warning(s"$genName rules did not match")
    
    info("")
    banner("Generation complete!")
