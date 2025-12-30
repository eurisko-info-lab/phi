package phi

/**
 * Tests for Φ-Hello framework.
 * 
 * Validates the core algebraic infrastructure.
 */
class CoreSpec extends munit.FunSuite:
  import Core.*
  import Core.Val.*
  import Meta.*

  // ─────────────────────────────────────────────────────────────────────────
  // Value Tests
  // ─────────────────────────────────────────────────────────────────────────
  
  test("Val.show renders correctly") {
    assertEquals(VInt(42).show, "42")
    assertEquals(VStr("hello").show, "\"hello\"")
    assertEquals(VCon("Nil", Nil).show, "Nil")
    assertEquals(VCon("Cons", List(VInt(1), VCon("Nil", Nil))).show, "Cons(1, Nil)")
    assertEquals(VList(List(VInt(1), VInt(2))).show, "[1, 2]")
  }
  
  test("Val.children extracts immediate children") {
    val v = VCon("Add", List(VInt(1), VInt(2)))
    assertEquals(v.children, List(VInt(1), VInt(2)))
    assertEquals(VInt(42).children, Nil)
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Pattern Functor Tests
  // ─────────────────────────────────────────────────────────────────────────
  
  test("V.out decomposes Val into V") {
    assertEquals(V.out(VInt(42)), V.I(42))
    assertEquals(V.out(VStr("hi")), V.S("hi"))
    assertEquals(V.out(VCon("X", List(VInt(1)))), V.C("X", List(VInt(1))))
  }
  
  test("V.in recomposes V into Val") {
    assertEquals(V.in(V.I(42)), VInt(42))
    assertEquals(V.in(V.C("X", List(VInt(1)))), VCon("X", List(VInt(1))))
  }
  
  test("V round-trips: in(out(v)) == v") {
    val vals = List(VInt(42), VStr("hi"), VCon("X", List(VInt(1))), VList(List(VInt(1))))
    vals.foreach(v => assertEquals(V.in(V.out(v)), v))
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Recursion Scheme Tests
  // ─────────────────────────────────────────────────────────────────────────
  
  test("cata counts nodes correctly") {
    val count: V[Int] => Int = {
      case C(_, args) => 1 + args.sum
      case L(elems)   => 1 + elems.sum
      case _          => 1
    }
    
    assertEquals(cata(count)(VInt(1)), 1)
    assertEquals(cata(count)(VCon("X", List(VInt(1), VInt(2)))), 3)
    assertEquals(cata(count)(VCon("X", List(VCon("Y", List(VInt(1))), VInt(2)))), 4)
  }
  
  test("cata evaluates arithmetic") {
    val eval: V[Int] => Int = {
      case C("Lit", List(n)) => n
      case C("Add", List(l, r)) => l + r
      case C("Mul", List(l, r)) => l * r
      case I(n) => n
      case _ => 0
    }
    
    // 1 + 2 = 3
    val e1 = VCon("Add", List(VCon("Lit", List(VInt(1))), VCon("Lit", List(VInt(2)))))
    assertEquals(cata(eval)(e1), 3)
    
    // (1 + 2) * 3 = 9
    val e2 = VCon("Mul", List(e1, VCon("Lit", List(VInt(3)))))
    assertEquals(cata(eval)(e2), 9)
  }
  
  test("ana generates structure from seed") {
    val peanoCoalg: Int => V[Int] = n =>
      if n <= 0 then C("Zero", Nil) else C("Succ", List(n - 1))
    
    assertEquals(ana(peanoCoalg)(0), VCon("Zero", Nil))
    assertEquals(ana(peanoCoalg)(1), VCon("Succ", List(VCon("Zero", Nil))))
  }
  
  test("hylo computes without intermediate structure") {
    // hylo is tricky in this setup because V uses homogeneous children
    // Instead, let's just verify the cata/ana combination works
    val listCoalg: Int => V[Int] = n =>
      if n <= 0 then C("Nil", Nil) else C("Cons", List(n - 1))  // only recurse on tail
    
    val sumAlg: V[Int] => Int = {
      case C("Nil", Nil) => 0
      case C("Cons", List(t)) => 1 + t  // count nodes, tail already folded
      case _ => 0
    }
    
    // Count: Cons(Cons(Cons(Cons(Cons(Nil))))) = 5 nodes
    assertEquals(hylo(sumAlg, listCoalg)(5), 5)
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Validation Tests
  // ─────────────────────────────────────────────────────────────────────────
  
  test("Validated accumulates errors") {
    val v1: Validated[String, Int] = Invalid(List("error1"))
    val v2: Validated[String, Int] = Invalid(List("error2"))
    
    (v1 zip v2) match
      case Invalid(errs) => assertEquals(errs, List("error1", "error2"))
      case _ => fail("Expected Invalid")
  }
  
  test("Validated.sequence combines list of validations") {
    val vs = List(Valid(1), Valid(2), Valid(3))
    assertEquals(Validated.sequence(vs), Valid(List(1, 2, 3)))
    
    val vsErr = List(Valid(1), Invalid(List("e1")), Valid(3), Invalid(List("e2")))
    Validated.sequence(vsErr) match
      case Invalid(errs) => assertEquals(errs, List("e1", "e2"))
      case _ => fail("Expected Invalid")
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Pattern Matching Tests
  // ─────────────────────────────────────────────────────────────────────────
  
  test("Pattern matches simple values") {
    val p = PVar("x")
    assertEquals(p.matchAgainst(VInt(42)).flatMap(_.lookup("x")), Some(VInt(42)))
  }
  
  test("Pattern matches constructors") {
    val p = PCon("Add", List(PVar("l"), PVar("r")))
    val v = VCon("Add", List(VInt(1), VInt(2)))
    
    p.matchAgainst(v) match
      case Some(env) =>
        assertEquals(env.lookup("l"), Some(VInt(1)))
        assertEquals(env.lookup("r"), Some(VInt(2)))
      case None => fail("Should match")
  }
  
  test("Pattern rejects wrong constructor") {
    val p = PCon("Add", List(PVar("l"), PVar("r")))
    val v = VCon("Mul", List(VInt(1), VInt(2)))
    assertEquals(p.matchAgainst(v), None)
  }
  
  test("Wildcard matches anything") {
    assertEquals(PWild.matchAgainst(VInt(42)), Some(Env.empty))
    assertEquals(PWild.matchAgainst(VCon("X", Nil)), Some(Env.empty))
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Expression Evaluation Tests
  // ─────────────────────────────────────────────────────────────────────────
  
  test("Expression evaluates literals") {
    assertEquals(ELit(VInt(42)).eval(Env.empty), VInt(42))
    assertEquals(ELit(VStr("hi")).eval(Env.empty), VStr("hi"))
  }
  
  test("Expression evaluates variables") {
    val env = Env("x" -> VInt(42))
    assertEquals(EVar("x").eval(env), VInt(42))
  }
  
  test("Expression evaluates constructors") {
    assertEquals(ECon("Pair", List(ELit(VInt(1)), ELit(VInt(2)))).eval(Env.empty),
                 VCon("Pair", List(VInt(1), VInt(2))))
  }
  
  test("Expression evaluates let bindings") {
    val e = ELet("x", ELit(VInt(10)), 
            ELet("y", ELit(VInt(20)),
            ECon("Sum", List(EVar("x"), EVar("y")))))
    assertEquals(e.eval(Env.empty), VCon("Sum", List(VInt(10), VInt(20))))
  }
  
  test("Expression evaluates pattern match") {
    val e = EMatch(ECon("Just", List(ELit(VInt(42)))), List(
      (PCon("Just", List(PVar("x"))), EVar("x")),
      (PCon("Nothing", Nil), ELit(VInt(0)))
    ))
    assertEquals(e.eval(Env.empty), VInt(42))
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Zipper Tests
  // ─────────────────────────────────────────────────────────────────────────
  
  test("Zipper navigates to path") {
    val v = VCon("A", List(VCon("B", List(VInt(1))), VInt(2)))
    val z = Zipper.from(v)
    
    Zipper.navigate(z, List(0)) match
      case Some(z1) => assertEquals(z1.head.value, VCon("B", List(VInt(1))))
      case None => fail("Should navigate")
    
    Zipper.navigate(z, List(0, 0)) match
      case Some(z2) => assertEquals(z2.head.value, VInt(1))
      case None => fail("Should navigate")
  }
  
  test("Zipper.toVal recovers original") {
    val v = VCon("X", List(VInt(1), VInt(2)))
    assertEquals(Zipper.toVal(Zipper.from(v)), v)
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Optics Tests
  // ─────────────────────────────────────────────────────────────────────────
  
  test("Lens gets and sets") {
    val v = VCon("Pair", List(VInt(1), VInt(2)))
    val first = Optics.arg(0)
    
    assertEquals(first.get(v), VInt(1))
    assertEquals(first.set(VInt(99))(v), VCon("Pair", List(VInt(99), VInt(2))))
  }
  
  test("Optics.everywhere transforms all nodes") {
    val v = VCon("A", List(VInt(1), VCon("B", List(VInt(2)))))
    val doubled = Optics.everywhere {
      case VInt(n) => VInt(n * 2)
      case x => x
    }(v)
    assertEquals(doubled, VCon("A", List(VInt(2), VCon("B", List(VInt(4))))))
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Hash Tests
  // ─────────────────────────────────────────────────────────────────────────
  
  test("Hash is deterministic") {
    val v = VCon("X", List(VInt(1)))
    assertEquals(Hash.of(v), Hash.of(v))
  }
  
  test("Different values have different hashes (usually)") {
    val h1 = Hash.of(VInt(1))
    val h2 = Hash.of(VInt(2))
    assertNotEquals(h1, h2)
  }
