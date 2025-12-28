package phi

import phi.Term.*
import phi.Syntax.*
import phi.Repo.*
import phi.HashConsing.*
import phi.Xform.*
import phi.Attributes.*
import phi.Grammar.*

/**
 * INTERTWINING RULES
 * 
 * This module demonstrates the complete end-to-end pipeline:
 * 
 *   parsing → Term[A] → repo → editor → changes → Xforms → typecheck → evaluation → rendering → repo
 * 
 * Each stage is connected to the next, with bidirectional transformations ensuring
 * consistency across all representations.
 */
object Pipeline:

  // ============================================================================
  // STAGE 1: Parsing - Text → Term[A]
  // ============================================================================
  
  /** Parse source text into a Term using a given syntax */
  def parse[A](syntax: Syntax[A], source: String): Either[String, Term[A]] =
    val tokens = Lexer.tokenize(source)
    syntax.parse(tokens) match
      case Some((term, remaining)) if remaining.isEmpty => Right(term)
      case Some((term, remaining)) => 
        Right(term) // Allow partial parse, remaining tokens ignored
      case None => Left(s"Failed to parse: $source")

  /** Parse LC expression from string */
  def parseLC(source: String): Either[String, Term[LC]] =
    parse(LCSyntax.lcSyntax, source)

  // ============================================================================
  // STAGE 2: Term[A] → Repository Storage
  // ============================================================================
  
  /** Store a term in the repository with hash-consing */
  def storeInRepo[A](repo: Repo[A], term: Term[A], message: String)(using hasher: TermHasher[A]): (Repo[A], Hash) =
    val hash = HashConsing.hashTerm(term)
    val change = Change.Replace(Term.Hole("root"), term)
    val patch = Patch(
      id = s"patch-${System.currentTimeMillis()}",
      changes = List(change),
      dependencies = Nil,
      metadata = Map("message" -> message, "hash" -> hash.toString)
    )
    (repo.addPatch(patch), hash)

  /** Retrieve a term from the repo by applying all patches */
  def retrieveFromRepo[A](repo: Repo[A]): Term[A] =
    repo.currentState

  // ============================================================================
  // STAGE 3: Repository → Editor (Zipper-based navigation)
  // ============================================================================
  
  /** Create an editor session from the current repo state */
  def openEditor[A](repo: Repo[A]): Editor[A] =
    val term = repo.currentState
    Editor(term, TermZipper.fromTerm(term), List.empty)

  /** Editor state with history tracking */
  case class Editor[A](
    original: Term[A],
    zipper: TermZipper[A],
    history: List[Change[A]]
  ):
    def currentTerm: Term[A] = zipper.toTerm
    
    def navigate(direction: Direction): Editor[A] =
      direction match
        case Direction.Up => 
          zipper.up match
            case Some(z) => copy(zipper = z)
            case None => this
        case Direction.Down(index) =>
          zipper.down(index) match
            case Some(z) => copy(zipper = z)
            case None => this
    
    def applyChange(change: Change[A]): Editor[A] =
      val newTerm = ChangeApplicator.apply(change, currentTerm)
      Editor(original, TermZipper.fromTerm(newTerm), change :: history)
    
    def fillHole(name: String, value: A): Editor[A] =
      val change = Change.Replace(Term.Hole(name), Term.Done(value))
      applyChange(change)

  enum Direction:
    case Up
    case Down(index: Int)

  // ============================================================================
  // STAGE 4: Editor Changes → Xform Application
  // ============================================================================
  
  /** Apply Xform to propagate changes across representations */
  def propagateViaXform[A, B](
    editor: Editor[A],
    xform: Xform[A, B]
  ): Either[String, Term[B]] =
    val term = editor.currentTerm
    termToB(term, xform)

  /** Transform Term[A] → Term[B] using an Xform */
  def termToB[A, B](term: Term[A], xform: Xform[A, B]): Either[String, Term[B]] =
    term match
      case Term.Done(a) =>
        xform.forward(a) match
          case Some(b) => Right(Term.Done(b))
          case None => Left(s"Xform forward failed for: $a")
      case Term.Hole(name) =>
        Right(Term.Hole(name)) // Holes propagate through

  /** Transform Term[B] → Term[A] (backward direction) */
  def termToA[A, B](term: Term[B], xform: Xform[A, B]): Either[String, Term[A]] =
    term match
      case Term.Done(b) =>
        xform.backward(b) match
          case Some(a) => Right(Term.Done(a))
          case None => Left(s"Xform backward failed for: $b")
      case Term.Hole(name) =>
        Right(Term.Hole(name))

  // ============================================================================
  // STAGE 5: Xform Output → Type Checking
  // ============================================================================
  
  /** Type check an LC term, returning typed version */
  def typeCheck(term: Term[LC]): Either[String, Term[TypedLC]] =
    term match
      case Term.Done(lc) =>
        TypeChecker.infer(lc, Map.empty) match
          case Some(typed) => Right(Term.Done(typed))
          case None => Left(s"Type checking failed for: $lc")
      case Term.Hole(name) =>
        Right(Term.Hole(name))

  /** Full LC → TypedLC Xform application with type checking */
  def lcToTypedLC(term: Term[LC]): Either[String, Term[TypedLC]] =
    typeCheck(term)

  // ============================================================================
  // STAGE 6: Type-Checked Term → Evaluation
  // ============================================================================
  
  /** Evaluate a typed LC term to a value */
  def evaluate(term: Term[TypedLC]): Either[String, Term[TypedLC]] =
    term match
      case Term.Done(typed) =>
        Right(Term.Done(evaluateTypedLC(typed)))
      case Term.Hole(name) =>
        Left(s"Cannot evaluate term with hole: $name")

  /** Evaluate typed LC to normal form */
  def evaluateTypedLC(typed: TypedLC): TypedLC =
    typed match
      case TypedLC.TVar(name, ty) => typed
      case TypedLC.TLam(param, paramTy, body, ty) => typed // Lambda is a value
      case TypedLC.TApp(func, arg, ty) =>
        val evalFunc = evaluateTypedLC(func)
        val evalArg = evaluateTypedLC(arg)
        evalFunc match
          case TypedLC.TLam(param, _, body, _) =>
            evaluateTypedLC(substituteTyped(body, param, evalArg))
          case _ => TypedLC.TApp(evalFunc, evalArg, ty)
      case TypedLC.TLet(name, value, body, ty) =>
        val evalValue = evaluateTypedLC(value)
        evaluateTypedLC(substituteTyped(body, name, evalValue))

  /** Substitute in typed LC */
  def substituteTyped(term: TypedLC, name: String, replacement: TypedLC): TypedLC =
    term match
      case TypedLC.TVar(n, ty) if n == name => replacement
      case TypedLC.TVar(_, _) => term
      case TypedLC.TLam(param, paramTy, body, ty) if param == name => term
      case TypedLC.TLam(param, paramTy, body, ty) =>
        TypedLC.TLam(param, paramTy, substituteTyped(body, name, replacement), ty)
      case TypedLC.TApp(func, arg, ty) =>
        TypedLC.TApp(
          substituteTyped(func, name, replacement),
          substituteTyped(arg, name, replacement),
          ty
        )
      case TypedLC.TLet(n, value, body, ty) =>
        val newValue = substituteTyped(value, name, replacement)
        val newBody = if n == name then body else substituteTyped(body, name, replacement)
        TypedLC.TLet(n, newValue, newBody, ty)

  // ============================================================================
  // STAGE 7: Evaluation Result → Rendering
  // ============================================================================
  
  /** Render a term back to source text */
  def render[A](term: Term[A], syntax: Syntax[A]): String =
    syntax.render(term).map(_.toString).mkString(" ")

  /** Render LC term to string */
  def renderLC(term: Term[LC]): String =
    render(term, LCSyntax.lcSyntax)

  /** Render TypedLC to string (showing types) */
  def renderTypedLC(term: Term[TypedLC]): String =
    term match
      case Term.Done(typed) => renderTypedLCValue(typed)
      case Term.Hole(name) => s"?$name"

  def renderTypedLCValue(typed: TypedLC): String =
    typed match
      case TypedLC.TVar(name, ty) => s"$name : ${renderType(ty)}"
      case TypedLC.TLam(param, paramTy, body, ty) =>
        s"(λ$param : ${renderType(paramTy)}. ${renderTypedLCValue(body)}) : ${renderType(ty)}"
      case TypedLC.TApp(func, arg, ty) =>
        s"(${renderTypedLCValue(func)} ${renderTypedLCValue(arg)}) : ${renderType(ty)}"
      case TypedLC.TLet(name, value, body, ty) =>
        s"let $name = ${renderTypedLCValue(value)} in ${renderTypedLCValue(body)} : ${renderType(ty)}"

  def renderType(ty: LCType): String =
    ty match
      case LCType.Base(name) => name
      case LCType.Arrow(from, to) => s"(${renderType(from)} → ${renderType(to)})"
      case LCType.Var(name) => s"'$name"

  // ============================================================================
  // STAGE 8: Rendered Output → Back to Repository
  // ============================================================================
  
  /** Commit the current editor state back to the repository */
  def commitToRepo[A](repo: Repo[A], editor: Editor[A], message: String)(using hasher: TermHasher[A]): Repo[A] =
    if editor.history.isEmpty then
      repo // No changes to commit
    else
      val patch = Patch(
        id = s"patch-${System.currentTimeMillis()}",
        changes = editor.history.reverse, // Apply in order
        dependencies = repo.branches.get("main").toList,
        metadata = Map("message" -> message)
      )
      repo.addPatch(patch)

  // ============================================================================
  // FULL PIPELINE: End-to-End Integration
  // ============================================================================
  
  /** 
   * Complete pipeline execution:
   * parsing → Term[A] → repo → editor → changes → Xforms → typecheck → evaluation → rendering → repo
   */
  case class PipelineResult(
    parsedTerm: Term[LC],
    storedHash: Hash,
    editorState: Editor[LC],
    transformedTerm: Term[TypedLC],
    evaluatedTerm: Term[TypedLC],
    renderedOutput: String,
    finalRepo: Repo[LC]
  )

  def runFullPipeline(
    source: String,
    edits: List[Change[LC]] = Nil,
    message: String = "Pipeline execution"
  ): Either[String, PipelineResult] =
    given TermHasher[LC] = LCHasher

    // Stage 1: Parse
    for
      parsedTerm <- parseLC(source)
      
      // Stage 2: Store in repo
      initialRepo = Repo.empty[LC]
      (repoWithTerm, hash) = storeInRepo(initialRepo, parsedTerm, "Initial parse")
      
      // Stage 3: Open editor
      editor0 = openEditor(repoWithTerm)
      
      // Stage 4: Apply edits
      editor1 = edits.foldLeft(editor0)((ed, change) => ed.applyChange(change))
      
      // Stage 5: Transform via Xform (LC → TypedLC)
      transformedTerm <- lcToTypedLC(editor1.currentTerm)
      
      // Stage 6: Evaluate
      evaluatedTerm <- evaluate(transformedTerm)
      
      // Stage 7: Render
      rendered = renderTypedLC(evaluatedTerm)
      
      // Stage 8: Commit back to repo
      finalRepo = commitToRepo(repoWithTerm, editor1, message)
      
    yield PipelineResult(
      parsedTerm = parsedTerm,
      storedHash = hash,
      editorState = editor1,
      transformedTerm = transformedTerm,
      evaluatedTerm = evaluatedTerm,
      renderedOutput = rendered,
      finalRepo = finalRepo
    )

  // ============================================================================
  // LC-specific Hasher
  // ============================================================================
  
  given LCHasher: TermHasher[LC] with
    def hash(value: LC): Hash =
      val content = value match
        case LC.Var(name) => s"var:$name"
        case LC.Lam(param, body) => s"lam:$param:${hash(body)}"
        case LC.App(func, arg) => s"app:${hash(func)}:${hash(arg)}"
        case LC.Let(name, value, body) => s"let:$name:${hash(value)}:${hash(body)}"
      HashConsing.computeHash(content)

  // ============================================================================
  // BIDIRECTIONAL PIPELINE VERIFICATION
  // ============================================================================
  
  /** Verify round-trip: A → B → A preserves semantics */
  def verifyRoundTrip[A, B](
    term: Term[A],
    xform: Xform[A, B]
  ): Either[String, Boolean] =
    for
      termB <- termToB(term, xform)
      termA2 <- termToA(termB, xform)
    yield term == termA2

  /** Verify that parsing and rendering are inverse operations */
  def verifyParseRender(source: String): Either[String, Boolean] =
    for
      term <- parseLC(source)
      rendered = renderLC(term)
      reparsed <- parseLC(rendered)
    yield term == reparsed

  // ============================================================================
  // PIPELINE STAGES AS XFORMS
  // ============================================================================
  
  /** Each pipeline stage can be viewed as an Xform */
  object StageXforms:
    
    /** Parse stage as Xform[String, Term[LC]] */
    val parseXform: Xform[String, Term[LC]] = new Xform[String, Term[LC]]:
      def forward(source: String): Option[Term[LC]] = parseLC(source).toOption
      def backward(term: Term[LC]): Option[String] = Some(renderLC(term))

    /** TypeCheck stage as Xform[Term[LC], Term[TypedLC]] */
    val typeCheckXform: Xform[Term[LC], Term[TypedLC]] = new Xform[Term[LC], Term[TypedLC]]:
      def forward(term: Term[LC]): Option[Term[TypedLC]] = typeCheck(term).toOption
      def backward(typed: Term[TypedLC]): Option[Term[LC]] =
        // Strip types to get back to LC
        typed match
          case Term.Done(t) => Some(Term.Done(stripTypes(t)))
          case Term.Hole(name) => Some(Term.Hole(name))

    /** Evaluate stage as Xform (one-way, evaluation is not reversible) */
    val evalXform: Xform[Term[TypedLC], Term[TypedLC]] = new Xform[Term[TypedLC], Term[TypedLC]]:
      def forward(term: Term[TypedLC]): Option[Term[TypedLC]] = evaluate(term).toOption
      def backward(term: Term[TypedLC]): Option[Term[TypedLC]] = Some(term) // Evaluation is not invertible

    /** Strip types from TypedLC to get LC */
    def stripTypes(typed: TypedLC): LC =
      typed match
        case TypedLC.TVar(name, _) => LC.Var(name)
        case TypedLC.TLam(param, _, body, _) => LC.Lam(param, stripTypes(body))
        case TypedLC.TApp(func, arg, _) => LC.App(stripTypes(func), stripTypes(arg))
        case TypedLC.TLet(name, value, body, _) => LC.Let(name, stripTypes(value), stripTypes(body))

  // ============================================================================
  // INTERACTIVE PIPELINE RUNNER
  // ============================================================================
  
  /** Run pipeline interactively with step-by-step output */
  def runInteractive(source: String): Unit =
    println(s"=== PIPELINE EXECUTION ===")
    println(s"Input: $source")
    println()
    
    runFullPipeline(source) match
      case Right(result) =>
        println(s"1. PARSED:")
        println(s"   ${result.parsedTerm}")
        println()
        println(s"2. STORED IN REPO:")
        println(s"   Hash: ${result.storedHash}")
        println()
        println(s"3. EDITOR STATE:")
        println(s"   Current: ${result.editorState.currentTerm}")
        println(s"   History: ${result.editorState.history.length} changes")
        println()
        println(s"4. TYPE CHECKED:")
        println(s"   ${result.transformedTerm}")
        println()
        println(s"5. EVALUATED:")
        println(s"   ${result.evaluatedTerm}")
        println()
        println(s"6. RENDERED:")
        println(s"   ${result.renderedOutput}")
        println()
        println(s"7. REPO STATE:")
        println(s"   Patches: ${result.finalRepo.patches.size}")
        println()
        println("=== PIPELINE COMPLETE ===")
        
      case Left(error) =>
        println(s"PIPELINE ERROR: $error")

  // ============================================================================
  // DEMONSTRATION
  // ============================================================================
  
  @main def runPipelineDemo(): Unit =
    // Demo 1: Identity function
    println("\n--- Demo 1: Identity Function ---")
    runInteractive("λx.x")
    
    // Demo 2: Application
    println("\n--- Demo 2: Application ---")
    runInteractive("(λx.x) y")
    
    // Demo 3: Let binding
    println("\n--- Demo 3: Let Binding ---")
    runInteractive("let id = λx.x in id")

end Pipeline
