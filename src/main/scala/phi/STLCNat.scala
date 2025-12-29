package phi

import LangDSL.*
import MetaPattern.*
import RewriteStrategy.*

/**
 * STLC + Nat defined in Phi's meta-language.
 * 
 * This is the Phi program from the user's example, implemented using
 * the meta-interpreter DSL.
 */
object STLCNat:
  
  val spec: LangSpec = language("Phi")
    // Sorts
    .sort("Type")
    .sort("Term")
    
    // Type constructors
    .con("Unit", "Type")
    .con("Nat", "Type")
    .con("Arr", (None -> LangType.SortRef("Type")), (None -> LangType.SortRef("Type")))("Type")
    
    // Term constructors
    .con("unit", "Term")
    .con("zero", "Term")
    .con("succ", (None -> LangType.SortRef("Term")))("Term")
    .con("lam", 
      (Some("x") -> LangType.SortRef("Term")),
      (None -> LangType.SortRef("Type")),
      (None -> LangType.SortRef("Term")))("Term")
    .con("app", (None -> LangType.SortRef("Term")), (None -> LangType.SortRef("Term")))("Term")
    .con("NatRec",
      (Some("n") -> LangType.SortRef("Term")),
      (Some("base") -> LangType.SortRef("Term")),
      (Some("step") -> LangType.SortRef("Term")))("Term")
    // Pairs
    .con("pair", (None -> LangType.SortRef("Term")), (None -> LangType.SortRef("Term")))("Term")
    .con("fst", (None -> LangType.SortRef("Term")))("Term")
    .con("snd", (None -> LangType.SortRef("Term")))("Term")
    
    // Beta reduction rule
    .rule("BetaReduce",
      // app (lam x A body) v ↦ body[x := v]
      cas(
        c("app", c("lam", v("x"), v("A"), v("body")), v("v")),
        subst(v("body"), "x", v("v"))
      )
    )
    
    // NatRec rules
    .rule("NatRecZero",
      // NatRec zero z s ↦ z
      cas(
        c("NatRec", c("zero"), v("z"), v("s")),
        v("z")
      )
    )
    
    .rule("NatRecSucc",
      // NatRec (succ n) z s ↦ app (app s n) (NatRec n z s)
      cas(
        c("NatRec", c("succ", v("n")), v("z"), v("s")),
        c("app", c("app", v("s"), v("n")), c("NatRec", v("n"), v("z"), v("s")))
      )
    )
    
    // Normalize strategy
    .strategy("normalize", 
      repeat(oneOf(apply("BetaReduce"), apply("NatRecZero"), apply("NatRecSucc"),
                   apply("FstPair"), apply("SndPair"))))
    
    // Pair projection rules
    .rule("FstPair", cas(c("fst", c("pair", v("a"), v("b"))), v("a")))
    .rule("SndPair", cas(c("snd", c("pair", v("a"), v("b"))), v("b")))
    
    // =========================================================================
    // Standard Library
    // =========================================================================
    
    // add m n = NatRec m n (λ_ : Nat. λacc : Nat. succ acc)
    .defn("add",
      c("lam", c("m"), c("Nat"),
        c("lam", c("n"), c("Nat"),
          c("NatRec", c("m"),
            c("n"),
            c("lam", c("_"), c("Nat"),
              c("lam", c("acc"), c("Nat"),
                c("succ", c("acc"))))))))
    
    // =========================================================================
    // Fibonacci (proper version using pairs)
    // =========================================================================
    
    // fibStep : (Nat, Nat) -> (Nat, Nat)
    // fibStep (a, b) = (b, a + b)
    .defn("fibStep",
      c("lam", c("p"), c("Nat"),  // p is really a pair but we fake the type
        c("pair",
          c("snd", c("p")),
          c("app", c("app", v("add"), c("fst", c("p"))), c("snd", c("p"))))))
    
    // fib n = fst (NatRec n (pair zero (succ zero)) (λ_. λp. fibStep p))
    .defn("fib",
      c("lam", c("fn"), c("Nat"),
        c("fst",
          c("NatRec", c("fn"),
            c("pair", c("zero"), c("succ", c("zero"))),
            c("lam", c("_k"), c("Nat"),
              c("lam", c("fp"), c("Nat"),
                c("app", v("fibStep"), c("fp"))))))))
    
    // =========================================================================  
    // Numerals
    // =========================================================================
    
    .defn("one",   c("succ", c("zero")))
    .defn("two",   c("succ", v("one")))
    .defn("three", c("succ", v("two")))
    .defn("four",  c("succ", v("three")))
    .defn("five",  c("succ", v("four")))
    .defn("six",   c("succ", v("five")))
    .defn("seven", c("succ", v("six")))
    .defn("eight", c("succ", v("seven")))
    .defn("nine",  c("succ", v("eight")))
    .defn("ten",   c("succ", v("nine")))
    
    // =========================================================================
    // Test
    // =========================================================================
    
    .defn("fib10", c("app", v("fib"), v("ten")))
    
    // Simple test: NatRec (succ zero) zero (λk. λprev. succ prev)
    // Should give: succ zero = 1
    .defn("test1", c("NatRec", c("succ", c("zero")), c("zero"),
      c("lam", c("k"), c("Nat"), c("lam", c("prev"), c("Nat"), c("succ", c("prev"))))))
    
    // add 2 3 = 5
    .defn("test_add", c("app", c("app", v("add"), v("two")), v("three")))
    
    // fib 5 = 5
    .defn("fib5", c("app", v("fib"), v("five")))
    
    // mult m n = NatRec m zero (λ_. λacc. add n acc)
    // Using distinct internal names to avoid capture
    .defn("mult",
      c("lam", c("mm"), c("Nat"),
        c("lam", c("nn"), c("Nat"),
          c("NatRec", c("mm"),
            c("zero"),
            c("lam", c("ii"), c("Nat"),
              c("lam", c("aa"), c("Nat"),
                c("app", c("app", v("add"), c("nn")), c("aa"))))))))
    
    // fact n = NatRec n (succ zero) (λk. λacc. mult (succ k) acc)
    .defn("fact",
      c("lam", c("fn"), c("Nat"),
        c("NatRec", c("fn"),
          c("succ", c("zero")),
          c("lam", c("fk"), c("Nat"),
            c("lam", c("fa"), c("Nat"),
              c("app", c("app", v("mult"), c("succ", c("fk"))), c("fa")))))))
    
    // fact 5 = 120
    .defn("fact5", c("app", v("fact"), v("five")))
    
    // mult 2 3 = 6
    .defn("mult23", c("app", c("app", v("mult"), v("two")), v("three")))
    
    // fib 7 = ?
    .defn("fib7", c("app", v("fib"), v("seven")))
    
    .build()

/** CLI to run the meta-interpreter */
@main def RunMeta(defName: String = "fib10"): Unit =
  println(s"=== Phi Meta-Interpreter ===")
  println(s"Language: ${STLCNat.spec.name}")
  println(s"Evaluating: $defName")
  println()
  
  val interp = LangInterpreter(STLCNat.spec)
  
  try
    val term = interp.evalDef(defName)
    println(s"Initial term: ${term.show}")
    println()
    println("Normalizing...")
    
    val start = System.currentTimeMillis()
    val result = interp.normalize(term)
    val elapsed = System.currentTimeMillis() - start
    val stepsPerSec = if elapsed > 0 then result.steps * 1000 / elapsed else 0
    
    println(s"Result: ${result.value.show}")
    println(s"Time: ${elapsed}ms, Steps: ${result.steps}, Rate: ${stepsPerSec} steps/s")
  catch
    case e: Exception =>
      println(s"Error: ${e.getMessage}")
      e.printStackTrace()
