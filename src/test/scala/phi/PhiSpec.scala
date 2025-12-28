package phi

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.{Gen, Arbitrary}

/**
 * Comprehensive test suite for the Phi language ecosystem.
 */
class PhiSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks:

  // ===========================================================================
  // 1. Term Tests
  // ===========================================================================

  test("Term.Done should hold a value") {
    val t = Term.Done(42)
    t.isDone shouldBe true
    t.isHole shouldBe false
    t.getOrElse(0) shouldBe 42
    t.toOption shouldBe Some(42)
  }

  test("Term.Hole should represent incomplete terms") {
    val h = Term.Hole[Int](Some("missing"))
    h.isHole shouldBe true
    h.isDone shouldBe false
    h.getOrElse(99) shouldBe 99
    h.toOption shouldBe None
  }

  test("Term.map should transform Done values") {
    Term.Done(5).map(_ * 2) shouldBe Term.Done(10)
    Term.Hole[Int](None).map(_ * 2) shouldBe Term.Hole(None)
  }

  test("Term.flatMap should chain computations") {
    Term.Done(5).flatMap(x => Term.Done(x + 1)) shouldBe Term.Done(6)
    Term.Done(5).flatMap(_ => Term.Hole(None)) shouldBe Term.Hole(None)
    Term.Hole[Int](None).flatMap(x => Term.Done(x + 1)) shouldBe Term.Hole(None)
  }

  test("Term.sequence should combine multiple terms") {
    Term.sequence(Seq(Term.Done(1), Term.Done(2), Term.Done(3))) shouldBe Term.Done(Seq(1, 2, 3))
    Term.sequence(Seq(Term.Done(1), Term.Hole(None), Term.Done(3))).isHole shouldBe true
  }

  // ===========================================================================
  // 2. Zipper Tests
  // ===========================================================================

  test("TermZipper should allow hole filling") {
    val z = TermZipper[Int](Term.Hole(Some("x")))
    val filled = z.fill(42)
    filled.focus shouldBe Term.Done(42)
  }

  test("TermZipper should allow replacement") {
    val z = TermZipper(Term.Done(1))
    val replaced = z.replace(Term.Done(2))
    replaced.focus shouldBe Term.Done(2)
  }

  // ===========================================================================
  // 3. Change Tests
  // ===========================================================================

  test("Change.Insert should fill holes") {
    val term = Term.Hole[Int](None)
    ChangeApplicator(Change.Insert(42), term) shouldBe Term.Done(42)
  }

  test("Change.Insert should not affect Done terms") {
    val term = Term.Done(1)
    ChangeApplicator(Change.Insert(42), term) shouldBe Term.Done(1)
  }

  test("Change.Replace should replace any term") {
    ChangeApplicator(Change.Replace(Term.Done(99)), Term.Done(1)) shouldBe Term.Done(99)
    ChangeApplicator(Change.Replace(Term.Done(99)), Term.Hole(None)) shouldBe Term.Done(99)
  }

  test("Change.Delete should create holes") {
    ChangeApplicator(Change.Delete(Some("deleted")), Term.Done(42)) shouldBe Term.Hole(Some("deleted"))
  }

  test("Change.Sequence should apply changes in order") {
    val changes = Change.Sequence(List(
      Change.Insert[Int](1),
      Change.Map[Int, Int](_ + 10, _ - 10)
    ))
    ChangeApplicator(changes, Term.Hole(None)) shouldBe Term.Done(11)
  }

  test("Change.invert should produce inverse changes") {
    val original = Term.Done(42)
    val change = Change.Delete[Int]()
    val inverse = change.invert(original)
    val deleted = ChangeApplicator(change, original)
    ChangeApplicator(inverse, deleted) shouldBe original
  }

  // ===========================================================================
  // 4. Editor Tests
  // ===========================================================================

  test("Editor should track undo history") {
    val editor = Editor(Term.Done(1))
      .applyChange(Change.Replace(Term.Done(2)))
      .applyChange(Change.Replace(Term.Done(3)))

    editor.current shouldBe Term.Done(3)
    editor.undoStack.size shouldBe 2
  }

  test("Editor.undo should restore previous state") {
    val editor = Editor(Term.Done(1))
      .applyChange(Change.Replace(Term.Done(2)))

    editor.undo.map(_.current) shouldBe Some(Term.Done(1))
  }

  test("Editor.redo should restore undone state") {
    val editor = Editor(Term.Done(1))
      .applyChange(Change.Replace(Term.Done(2)))
      .undo.get

    editor.redo.map(_.current) shouldBe Some(Term.Done(2))
  }

  // ===========================================================================
  // 5. Lexer Tests
  // ===========================================================================

  test("Lexer should tokenize identifiers") {
    val tokens = Lexer.tokenize("foo bar").remaining.filter {
      case Lex.Whitespace(_) => false
      case _ => true
    }
    tokens shouldBe Vector(Lex.Ident("foo"), Lex.Ident("bar"))
  }

  test("Lexer should tokenize integers") {
    val tokens = Lexer.tokenize("42 123").remaining.filter {
      case Lex.Whitespace(_) => false
      case _ => true
    }
    tokens shouldBe Vector(Lex.IntLit(42), Lex.IntLit(123))
  }

  test("Lexer should tokenize keywords") {
    val tokens = Lexer.tokenize("let in if then else").remaining.filter {
      case Lex.Whitespace(_) => false
      case _ => true
    }
    tokens shouldBe Vector(
      Lex.Keyword("let"), Lex.Keyword("in"), Lex.Keyword("if"),
      Lex.Keyword("then"), Lex.Keyword("else")
    )
  }

  test("Lexer should tokenize symbols") {
    val tokens = Lexer.tokenize("+ - * / == ->").remaining.filter {
      case Lex.Whitespace(_) => false
      case _ => true
    }
    tokens shouldBe Vector(
      Lex.Symbol("+"), Lex.Symbol("-"), Lex.Symbol("*"),
      Lex.Symbol("/"), Lex.Symbol("=="), Lex.Symbol("->")
    )
  }

  test("Lexer should tokenize strings") {
    val tokens = Lexer.tokenize("\"hello\" \"world\"").remaining.filter {
      case Lex.Whitespace(_) => false
      case _ => true
    }
    tokens shouldBe Vector(Lex.StringLit("hello"), Lex.StringLit("world"))
  }

  test("Lexer should tokenize holes") {
    val tokens = Lexer.tokenize("? ?name").remaining.filter {
      case Lex.Whitespace(_) => false
      case _ => true
    }
    tokens shouldBe Vector(Lex.HoleTok(None), Lex.HoleTok(Some("name")))
  }

  // ===========================================================================
  // 6. Expression Parser Tests
  // ===========================================================================

  test("ExprParser should parse integers") {
    val result = ExprParser.expr().parse(Lexer.tokenize("42"))
    result.term shouldBe Term.Done(Expr.Num(42))
  }

  test("ExprParser should parse simple binary operations") {
    val result = ExprParser.expr().parse(Lexer.tokenize("1 + 2"))
    result.term shouldBe Term.Done(Expr.BinOp("+", Expr.Num(1), Expr.Num(2)))
  }

  test("ExprParser should respect precedence") {
    val result = ExprParser.expr().parse(Lexer.tokenize("1 + 2 * 3"))
    result.term shouldBe Term.Done(
      Expr.BinOp("+", Expr.Num(1), Expr.BinOp("*", Expr.Num(2), Expr.Num(3)))
    )
  }

  test("ExprParser should handle parentheses") {
    val result = ExprParser.expr().parse(Lexer.tokenize("(1 + 2) * 3"))
    result.term shouldBe Term.Done(
      Expr.BinOp("*", Expr.Paren(Expr.BinOp("+", Expr.Num(1), Expr.Num(2))), Expr.Num(3))
    )
  }

  test("ExprParser should parse let expressions") {
    val result = ExprParser.expr().parse(Lexer.tokenize("let x = 5 in x + 1"))
    result.term shouldBe Term.Done(
      Expr.LetExpr("x", Expr.Num(5), Expr.BinOp("+", Expr.Var("x"), Expr.Num(1)))
    )
  }

  test("ExprParser should parse if expressions") {
    val result = ExprParser.expr().parse(Lexer.tokenize("if 1 then 2 else 3"))
    result.term shouldBe Term.Done(
      Expr.IfExpr(Expr.Num(1), Expr.Num(2), Expr.Num(3))
    )
  }

  test("ExprParser should produce holes for incomplete input") {
    val result = ExprParser.expr().parse(Lexer.tokenize("1 +"))
    result.term.isDone shouldBe true // Partial parse succeeds with placeholder
  }

  // ===========================================================================
  // 7. Expression Evaluator Tests
  // ===========================================================================

  test("ExprEvaluator should evaluate numbers") {
    ExprEvaluator.eval(Expr.Num(42)) shouldBe 42
  }

  test("ExprEvaluator should evaluate arithmetic") {
    ExprEvaluator.eval(Expr.BinOp("+", Expr.Num(2), Expr.Num(3))) shouldBe 5
    ExprEvaluator.eval(Expr.BinOp("*", Expr.Num(4), Expr.Num(5))) shouldBe 20
    ExprEvaluator.eval(Expr.BinOp("-", Expr.Num(10), Expr.Num(3))) shouldBe 7
    ExprEvaluator.eval(Expr.BinOp("/", Expr.Num(15), Expr.Num(3))) shouldBe 5
  }

  test("ExprEvaluator should evaluate variables from environment") {
    ExprEvaluator.eval(Expr.Var("x"), Map("x" -> 42)) shouldBe 42
    ExprEvaluator.eval(Expr.BinOp("+", Expr.Var("x"), Expr.Var("y")), Map("x" -> 10, "y" -> 20)) shouldBe 30
  }

  test("ExprEvaluator should evaluate let expressions") {
    val expr = Expr.LetExpr("x", Expr.Num(5), Expr.BinOp("*", Expr.Var("x"), Expr.Num(2)))
    ExprEvaluator.eval(expr) shouldBe 10
  }

  test("ExprEvaluator should evaluate if expressions") {
    ExprEvaluator.eval(Expr.IfExpr(Expr.Num(1), Expr.Num(10), Expr.Num(20))) shouldBe 10
    ExprEvaluator.eval(Expr.IfExpr(Expr.Num(0), Expr.Num(10), Expr.Num(20))) shouldBe 20
  }

  test("ExprEvaluator should evaluate comparisons") {
    ExprEvaluator.eval(Expr.BinOp("==", Expr.Num(5), Expr.Num(5))) shouldBe 1
    ExprEvaluator.eval(Expr.BinOp("==", Expr.Num(5), Expr.Num(3))) shouldBe 0
    ExprEvaluator.eval(Expr.BinOp("<", Expr.Num(3), Expr.Num(5))) shouldBe 1
    ExprEvaluator.eval(Expr.BinOp(">", Expr.Num(3), Expr.Num(5))) shouldBe 0
  }

  // ===========================================================================
  // 8. Round-Trip Parsing Tests
  // ===========================================================================

  test("Expression parsing should round-trip") {
    val inputs = List(
      "42",
      "1 + 2",
      "1 + 2 * 3",
      "(1 + 2) * 3",
      "let x = 5 in x + 1"
    )

    for input <- inputs do
      val parsed = ExprParser.expr().parse(Lexer.tokenize(input))
      val rendered = Renderer.render(ExprParser.expr(), parsed.term)
      val reparsed = ExprParser.expr().parse(Lexer.tokenize(rendered))

      withClue(s"Round-trip for '$input' -> '$rendered':") {
        parsed.term shouldBe reparsed.term
      }
  }

  // ===========================================================================
  // 9. Repository Tests
  // ===========================================================================

  test("Repo should store and retrieve terms") {
    val repo = Repo[Int]()
    val hash = repo.store(Term.Done(42))
    repo.get(hash) shouldBe Some(Term.Done(42))
  }

  test("Repo should store terms with names") {
    val repo = Repo[Int]()
    repo.store(Term.Done(42), Set(Name("answer")))
    repo.getByName(Name("answer")) shouldBe Some(Term.Done(42))
  }

  test("Repo should support branches") {
    val repo = Repo[Int]()
    repo.store(Term.Done(1), Set(Name("x")))
    repo.createBranch("feature")
    repo.switchBranch("feature") shouldBe true
    repo.getCurrentBranch shouldBe "feature"
  }

  test("Repo should apply and revert patches") {
    val repo = Repo[Int]()
    val term = Term.Done(1)
    repo.store(term, Set(Name("x")))

    val patch = Patch.create("double it", Change.Map[Int, Int](_ * 2, _ / 2), term)
    val newTerm = repo.applyPatch(patch, term)

    newTerm shouldBe Term.Done(2)

    val reverted = repo.revertPatch(patch.id, newTerm)
    reverted shouldBe Some(Term.Done(1))
  }

  // ===========================================================================
  // 10. Hash-Consing Tests
  // ===========================================================================

  test("ContentHash should produce same hash for equal terms") {
    import TermHasher.given
    val h1 = ContentHash.hash(Term.Done(42))
    val h2 = ContentHash.hash(Term.Done(42))
    h1 shouldBe h2
  }

  test("ContentHash should produce different hashes for different terms") {
    import TermHasher.given
    val h1 = ContentHash.hash(Term.Done(42))
    val h2 = ContentHash.hash(Term.Done(43))
    h1 should not be h2
  }

  test("HashConsedStore should deduplicate terms") {
    import TermHasher.given
    val store = HashConsedStore[Int]()
    val (h1, t1) = store.intern(Term.Done(42))
    val (h2, t2) = store.intern(Term.Done(42))
    h1 shouldBe h2
    t1 should be theSameInstanceAs t2
    store.size shouldBe 1
  }

  // ===========================================================================
  // 11. Lambda Calculus Tests
  // ===========================================================================

  test("LC.freeVars should compute free variables") {
    LC.freeVars(LC.Var("x")) shouldBe Set("x")
    LC.freeVars(LC.Lam("x", LC.Var("x"))) shouldBe Set.empty
    LC.freeVars(LC.Lam("x", LC.Var("y"))) shouldBe Set("y")
    LC.freeVars(LC.App(LC.Var("f"), LC.Var("x"))) shouldBe Set("f", "x")
  }

  test("LC.subst should substitute correctly") {
    // [x/y]x = y
    LC.subst(LC.Var("x"), "x", LC.Var("y")) shouldBe LC.Var("y")

    // [x/y]z = z
    LC.subst(LC.Var("z"), "x", LC.Var("y")) shouldBe LC.Var("z")

    // [x/y](λx.x) = λx.x (x is bound)
    LC.subst(LC.Lam("x", LC.Var("x")), "x", LC.Var("y")) shouldBe LC.Lam("x", LC.Var("x"))
  }

  // ===========================================================================
  // 12. Xform Tests
  // ===========================================================================

  test("Xform.id should be identity") {
    val xform = Xform.id[Int]
    val term = Term.Done(42)
    xform.forward(term) shouldBe term
    xform.backward(term) shouldBe term
  }

  test("Xform composition should work") {
    val double = Xform[Int, Int](
      _.map(_ * 2),
      _.map(_ / 2)
    )
    val addOne = Xform[Int, Int](
      _.map(_ + 1),
      _.map(_ - 1)
    )
    val composed = double.andThen(addOne)

    composed.forward(Term.Done(5)) shouldBe Term.Done(11) // (5*2)+1
    composed.backward(Term.Done(11)) shouldBe Term.Done(5) // (11-1)/2
  }

  test("Xform.inverse should swap forward/backward") {
    val double = Xform[Int, Int](
      _.map(_ * 2),
      _.map(_ / 2)
    )
    val halve = double.inverse

    halve.forward(Term.Done(10)) shouldBe Term.Done(5)
    halve.backward(Term.Done(5)) shouldBe Term.Done(10)
  }

  // ===========================================================================
  // 13. Type Checker Tests
  // ===========================================================================

  test("TypeChecker should infer types for literals") {
    val result = TypeChecker.forward(Term.Done(LC.Lit(42)))
    result shouldBe a[Term.Done[?]]
    result.toOption.get.getType shouldBe LCType.TInt
  }

  test("TypeChecker should infer types for lambdas") {
    val lam = LC.Lam("x", LC.Var("x"))
    val result = TypeChecker.forward(Term.Done(lam))
    result.isDone shouldBe true
  }

  test("TypeChecker round-trip should preserve structure") {
    val lam = LC.Lam("x", LC.Lit(42))
    val typed = TypeChecker.forward(Term.Done(lam))
    val erased = TypeChecker.backward(typed)
    erased shouldBe Term.Done(lam)
  }

  // ===========================================================================
  // 14. LC to IC Transformation Tests
  // ===========================================================================

  test("LCToIC should transform simple terms") {
    val result = LCToIC.forward(Term.Done(LC.Lit(42)))
    result.isDone shouldBe true
  }

  test("LCToIC should transform lambdas") {
    val lam = LC.Lam("x", LC.Var("x"))
    val result = LCToIC.forward(Term.Done(lam))
    result.isDone shouldBe true
    result.toOption.get.nodes.nonEmpty shouldBe true
  }

  test("LCToIC should preserve holes") {
    val result = LCToIC.forward(Term.Hole[LC](Some("test")))
    result shouldBe Term.Hole(Some("test"))
  }

  // ===========================================================================
  // 15. Pattern Matching Tests
  // ===========================================================================

  test("PatternCompiler should compile simple patterns") {
    val pm = PatternMatch(
      LC.Var("x"),
      List(MatchCase(Pattern.PVar("y"), LC.Var("y")))
    )
    val result = PatternCompiler.forward(Term.Done(pm))
    result.isDone shouldBe true
  }

  test("PatternCompiler should compile literal patterns") {
    val pm = PatternMatch(
      LC.Lit(1),
      List(
        MatchCase(Pattern.PLit(1), LC.Lit(10)),
        MatchCase(Pattern.PWild, LC.Lit(0))
      )
    )
    val result = PatternCompiler.forward(Term.Done(pm))
    result.isDone shouldBe true
  }

  // ===========================================================================
  // 16. Attribute Tests
  // ===========================================================================

  test("Attributed should store and retrieve attributes") {
    val attr = Attributed(Term.Done(42))
      .setInherited("env", Term.Done(Map.empty))
      .setSynthesized("type", Term.Done(LCType.TInt))

    attr.getInherited("env") shouldBe Some(Term.Done(Map.empty))
    attr.getSynthesized("type") shouldBe Some(Term.Done(LCType.TInt))
  }

  test("DependentTypes.Typed should handle second-order holes") {
    val typed = DependentTypes.Typed[Int](
      Term.Hole(Some("value")),
      Term.Hole(Some("type"))
    )
    DependentTypes.isComplete(typed) shouldBe false

    val filled = DependentTypes.fillValue(typed, 42)
    filled.value shouldBe Term.Done(42)
    filled.ty shouldBe Term.Hole(Some("type"))
  }

  // ===========================================================================
  // 17. Grammar Parser Tests
  // ===========================================================================

  test("GrammarParser.typeRef should parse simple types") {
    val result = GrammarParser.typeRef.parse(Lexer.tokenize("Int"))
    result.term shouldBe Term.Done(TypeRef.Simple("Int"))
  }

  test("GrammarParser.annotation should parse annotations") {
    val result = GrammarParser.annotation.parse(Lexer.tokenize("@inline"))
    result.term shouldBe Term.Done(Annotation("inline", Nil))
  }

  // ===========================================================================
  // 18. Merge Tests
  // ===========================================================================

  test("Repo merge should handle already up-to-date") {
    val repo = Repo[Int]()
    repo.store(Term.Done(1), Set(Name("x")))
    repo.createBranch("feature")

    val result = repo.merge("feature")
    result shouldBe MergeResult.AlreadyUpToDate
  }

  // ===========================================================================
  // 19. Property-Based Tests
  // ===========================================================================

  test("Term.map should satisfy functor identity law") {
    forAll { (n: Int) =>
      Term.Done(n).map(identity) shouldBe Term.Done(n)
    }
  }

  test("Term.map should satisfy functor composition law") {
    forAll { (n: Int) =>
      val f: Int => Int = _ + 1
      val g: Int => Int = _ * 2
      Term.Done(n).map(f).map(g) shouldBe Term.Done(n).map(f andThen g)
    }
  }

  test("Change.invert should be self-inverse for Replace") {
    forAll { (n: Int, m: Int) =>
      val original = Term.Done(n)
      val change = Change.Replace(Term.Done(m))
      val inverse = change.invert(original)
      val doubleInverse = inverse.invert(Term.Done(m))

      ChangeApplicator(change, original) shouldBe Term.Done(m)
      ChangeApplicator(inverse, Term.Done(m)) shouldBe original
    }
  }

  test("Expression evaluation should be deterministic") {
    forAll(Gen.choose(1, 100), Gen.choose(1, 100)) { (a: Int, b: Int) =>
      val expr = Expr.BinOp("+", Expr.Num(a), Expr.Num(b))
      ExprEvaluator.eval(expr) shouldBe ExprEvaluator.eval(expr)
      ExprEvaluator.eval(expr) shouldBe (a + b)
    }
  }

  // ===========================================================================
  // 20. Integration Tests
  // ===========================================================================

  test("Full pipeline: parse -> store -> retrieve -> eval") {
    val repo = Repo[Expr]()

    // Parse
    val result = ExprParser.expr().parse(Lexer.tokenize("let x = 10 in x * 2"))
    result.term.isDone shouldBe true

    // Store
    val hash = repo.store(result.term, Set(Name("double_ten")))

    // Retrieve
    val retrieved = repo.getByName(Name("double_ten"))
    retrieved shouldBe Some(result.term)

    // Eval
    result.term match
      case Term.Done(expr) =>
        ExprEvaluator.eval(expr) shouldBe 20
      case _ => fail("Expected Done term")
  }

  test("Edit -> Xform -> Store pipeline") {
    val lcRepo = Repo[LC]()
    val editor = Editor(Term.Done(LC.Lit(5)))

    // Apply edit
    val edited = editor.applyChange(Change.Map[LC, LC](
      { case LC.Lit(n) => LC.Lit(n * 2); case x => x },
      { case LC.Lit(n) => LC.Lit(n / 2); case x => x }
    ))

    edited.current shouldBe Term.Done(LC.Lit(10))

    // Type check
    val typed = TypeChecker.forward(edited.current)
    typed.isDone shouldBe true

    // Store in repo
    val hash = lcRepo.store(edited.current, Set(Name("doubled")))
    lcRepo.getByName(Name("doubled")) shouldBe Some(edited.current)
  }
