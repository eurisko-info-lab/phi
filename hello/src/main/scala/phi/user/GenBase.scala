package phi.user

import java.nio.file.{Files, Paths, Path}
import scala.io.StdIn
import phi.phi.*
import phi.meta.{Val, LangInterpreter}
import phi.meta.Val.*

/** 
 * GenBase: Common utilities for code generators (GenMeta, GenPhi)
 * 
 * Provides:
 * - File loading and writing with diff comparison
 * - Val conversion utilities (toCons, patternToVal, etc.)
 * - Consistent output formatting
 *
 * Flags (set via env vars or parseArgs):
 *   PHI_VERBOSE=true  - detailed output
 *   PHI_DUMP_AST=true - dump AST during generation
 *   --write           - actually write to hand-crafted files (default: dry-run)
 */
object GenBase:
  
  /** Verbosity control - set to false to suppress detailed output */
  var verbose: Boolean = sys.env.get("PHI_VERBOSE").exists(_.toLowerCase == "true")
  
  /** Enable AST dumping for debugging */
  var dumpAst: Boolean = sys.env.get("PHI_DUMP_AST").exists(_.toLowerCase == "true")
  
  /** Dry-run mode (default true) - when true, don't modify hand-crafted files */
  var dryRun: Boolean = true
  
  /** Parse common args: --write disables dry-run, --verbose enables verbose */
  def parseArgs(args: Array[String]): Unit =
    if args.contains("--write") then dryRun = false
    if args.contains("--verbose") || args.contains("-v") then verbose = true
    if args.contains("--dump-ast") then dumpAst = true

  // ---------------------------------------------------------------------------
  // Spec Loading
  // ---------------------------------------------------------------------------
  
  def loadSpec(path: String): Either[String, LangSpec] =
    val src = Files.readString(Paths.get(path))
    PhiParser.parseSpec(src)
  
  def loadSpecOrDie(path: String, name: String): LangSpec =
    loadSpec(path) match
      case Right(s) => s
      case Left(err) => 
        println(s"Parse error in $name: $err")
        sys.exit(1)

  // ---------------------------------------------------------------------------
  // Output Utilities
  // ---------------------------------------------------------------------------
  
  def banner(title: String): Unit =
    println("=" * 60)
    println(title)
    println("=" * 60)
  
  def info(msg: String): Unit = println(msg)
  def detail(msg: String): Unit = if verbose then println(s"    $msg")
  def success(msg: String): Unit = println(s"    ✓ $msg")
  def warning(msg: String): Unit = println(s"    ⚠ $msg")
  def error(msg: String): Unit = println(s"    ✗ $msg")

  // ---------------------------------------------------------------------------
  // File Output with Diff Comparison
  // ---------------------------------------------------------------------------
  
  /** Write generated code, comparing with existing if present.
   *  In dry-run mode (default), only reports diffs without modifying hand-crafted files.
   *  Use --write to actually overwrite hand-crafted files.
   */
  def writeWithDiff(
    code: String,
    fileName: String,
    handCraftedDir: Path,
    generatedDir: Path
  ): Unit =
    val generatedPath = generatedDir.resolve(fileName)
    val handCraftedPath = handCraftedDir.resolve(fileName)
    
    // Ensure output directory exists
    if !Files.exists(generatedDir) then Files.createDirectories(generatedDir)
    
    // Check if hand-crafted version exists and compare
    if Files.exists(handCraftedPath) then
      val existing = Files.readString(handCraftedPath)
      if existing.trim != code.trim then
        warning(s"Diff detected between generated and hand-crafted $fileName")
        if verbose then showDiff(existing, code)
        
        if dryRun then
          info(s"    (dry-run) Would overwrite: $handCraftedPath")
          info(s"    Use --write to actually overwrite")
        else
          Files.writeString(handCraftedPath, code)
          success(s"Overwritten: $handCraftedPath")
      else
        success(s"Generated matches hand-crafted $fileName")
    else
      // No existing file - always write
      Files.writeString(handCraftedPath, code)
      success(s"Created: $handCraftedPath")
    
    // Always write to generated dir for reference
    Files.writeString(generatedPath, code)
    detail(s"Written to: $generatedPath")

  /** Show a simple diff between existing and generated code */
  def showDiff(existing: String, generated: String): Unit =
    val existingLines = existing.linesIterator.toVector
    val generatedLines = generated.linesIterator.toVector
    
    println(s"    --- existing (${existingLines.length} lines)")
    println(s"    +++ generated (${generatedLines.length} lines)")
    
    var diffCount = 0
    val maxDiffs = 10
    for i <- 0 until math.max(existingLines.length, generatedLines.length) if diffCount < maxDiffs do
      val existLine = existingLines.lift(i).getOrElse("")
      val genLine = generatedLines.lift(i).getOrElse("")
      if existLine != genLine then
        diffCount += 1
        if existLine.nonEmpty then println(s"    - ${i+1}: $existLine")
        if genLine.nonEmpty then println(s"    + ${i+1}: $genLine")
    
    if diffCount >= maxDiffs then
      println(s"    ... and more differences")

  // ---------------------------------------------------------------------------
  // Val Conversion Utilities
  // ---------------------------------------------------------------------------
  
  /** Convert Scala List to Cons/Nil Val representation */
  def toCons(list: List[Val]): Val = list match
    case Nil => VCon("Nil", Nil)
    case head :: tail => VCon("Cons", List(head, toCons(tail)))
  
  /** Convert MetaPattern to Val */
  def patternToVal(p: MetaPattern): Val = p match
    case MetaPattern.PVar(n) => VCon("PVar", List(VStr(n)))
    case MetaPattern.PCon(n, args) => VCon("PCon", List(VStr(n), toCons(args.map(patternToVal))))
    case MetaPattern.PApp(f, a) => VCon("PApp", List(patternToVal(f), patternToVal(a)))
    case MetaPattern.PSubst(b, v, r) => VCon("PSubst", List(patternToVal(b), VStr(v), patternToVal(r)))

  /** Convert Rule to Val */
  def ruleToVal(rule: Rule): Val =
    VCon("Rule", List(
      VStr(rule.name),
      toCons(rule.cases.map(c => VCon("RuleCase", List(patternToVal(c.pattern), patternToVal(c.body)))))
    ))

  /** Convert Xform (with spec context for associated rules) to Val */
  def xformToVal(spec: LangSpec, xform: Xform): Val =
    val rules = spec.rules.filter(_.name.startsWith(s"${xform.name}."))
    VCon("Xform", List(
      VStr(xform.name),
      toCons(xform.params.map { case (n, t) => VCon("Param", List(VStr(n), VStr(t))) }),
      VStr(xform.srcType),
      VStr(xform.tgtType),
      toCons(rules.map(ruleToVal))
    ))
