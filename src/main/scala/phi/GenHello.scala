package phi

import java.nio.file.{Files, Paths}
import Val.*

/**
 * Hello World Bootstrap Demo
 * 
 * Demonstrates the minimal bootstrapping cycle:
 * 1. Parse a .phi spec file (hello.phi)
 * 2. Use its grammar to parse input
 * 3. Load transform spec (hello2scala.phi)
 * 4. Apply transforms to generate Scala output
 * 
 * Usage: sbt "runMain phi.GenHello"
 */
@main def GenHello(): Unit =
  println("=== Phi Bootstrap Demo ===\n")
  
  // Step 1: Load the hello.phi spec
  println("Step 1: Loading hello.phi spec...")
  val helloSpec = parseSpecWithInheritance("examples/hello.phi") match
    case Right(s) => s
    case Left(err) => 
      println(s"Error: $err")
      sys.exit(1)
  
  println(s"  Spec name: ${helloSpec.name}")
  println(s"  Sorts: ${helloSpec.sorts.map(_.name).mkString(", ")}")
  println(s"  Constructors: ${helloSpec.constructors.map(_.name).mkString(", ")}")
  println(s"  Grammars: ${helloSpec.grammars.keys.mkString(", ")}")
  println()
  
  // Step 2: Load hello2scala.phi transform spec
  println("Step 2: Loading hello2scala.phi transforms...")
  val xformSpec = parseSpecWithInheritance("examples/hello2scala.phi") match
    case Right(s) => s
    case Left(err) =>
      println(s"Error loading transforms: $err")
      sys.exit(1)
  
  println(s"  Transform rules: ${xformSpec.rules.map(_.name).mkString(", ")}")
  println()
  
  // Step 3: Create a parser from the grammar
  println("Step 3: Creating parser from grammar...")
  val parser = GrammarInterp.specParser(helloSpec)
  
  // Step 4: Create interpreter for transforms
  val interpreter = new LangInterpreter(xformSpec)
  val greeting2scalaCases = xformSpec.rules
    .filter(_.name.startsWith("Greeting2Scala."))
    .flatMap(_.cases)
  
  // Step 5: Parse and transform inputs
  val inputs = List("Hello World", "Hello Phi", "Hello Bootstrap")
  println("Step 4: Parsing and transforming inputs...\n")
  
  for input <- inputs do
    println(s"  Input: \"$input\"")
    val ast = parser.parse("greeting", input)
    println(s"  AST:   ${ast}")
    
    ast match
      case Term.Done(v) =>
        // Apply Greeting2Scala transform
        interpreter.applyRule(v, greeting2scalaCases) match
          case Some(scalaAst) =>
            println(s"  Scala AST: ${scalaAst.show}")
            val code = renderScalaExpr(scalaAst)
            println(s"  Code: $code")
          case None =>
            // Fallback to manual rendering
            val output = renderGreeting(v)
            println(s"  Code (fallback): $output")
      case Term.Hole(_) =>
        println(s"  Parse failed!")
    println()
  
  // Step 6: Generate a complete Scala file
  println("Step 5: Generating Scala output...")
  val sb = new StringBuilder
  sb.append("// Generated from hello.phi via hello2scala.phi\n")
  sb.append("package hello\n\n")
  sb.append("object Main extends App {\n")
  for input <- inputs do
    val ast = parser.parse("greeting", input)
    ast match
      case Term.Done(v) =>
        interpreter.applyRule(v, greeting2scalaCases) match
          case Some(scalaAst) =>
            sb.append(s"  ${renderScalaExpr(scalaAst)}\n")
          case None =>
            sb.append(s"  ${renderGreeting(v)}\n")
      case _ => ()
  sb.append("}\n")
  
  val outFile = Paths.get("tmp/Hello.scala")
  Files.createDirectories(outFile.getParent)
  Files.writeString(outFile, sb.toString)
  println(s"  Generated: $outFile")
  println()
  
  // Show the generated file
  println("Generated code:")
  println("-" * 40)
  println(sb.toString)
  println("-" * 40)
  println("\n=== Bootstrap Demo Complete ===")

/** Render Scala AST to code string */
def renderScalaExpr(v: Val): String = v match
  case VCon("PrintLn", List(expr)) =>
    s"println(${renderScalaExpr(expr)})"
  case VCon("StringLit", List(VCon(s, Nil))) =>
    s"\"$s\""
  case VCon("StringConcat", List(l, r)) =>
    s"${renderScalaExpr(l)} + ${renderScalaExpr(r)}"
  case VCon(name, Nil) =>
    s"\"$name\""
  case other =>
    s"/* ${other.show} */"

/** Simple renderer for Greeting AST (fallback) */
def renderGreeting(v: Val): String = v match
  case VCon("Hello", List(VCon("SimpleName", List(VCon(name, Nil))))) =>
    s"println(\"Hello, $name!\")"
  case VCon("Hello", List(VCon(name, Nil))) =>
    s"println(\"Hello, $name!\")"
  case other =>
    s"// Unknown: ${other.show}"
