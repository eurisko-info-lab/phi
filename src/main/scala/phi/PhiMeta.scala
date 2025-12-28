package phi

/**
 * Phi as a meta-language: The Phi ecosystem expressed in Phi terms.
 * 
 * This demonstrates how Phi grammars, parsers, Xforms, type-checks,
 * and edits can themselves be represented as Phi terms, enabling
 * meta-programming and bootstrapping.
 */
object PhiMeta:

  // ===========================================================================
  // Meta-Term: Representing Phi Terms as Terms
  // ===========================================================================

  /**
   * Meta-level representation of Term structure.
   */
  enum MetaTerm:
    case Done(value: MetaValue)
    case Hole(label: Option[String])

  /**
   * Meta-level values that can be stored in terms.
   */
  enum MetaValue:
    case MInt(n: Int)
    case MString(s: String)
    case MBool(b: Boolean)
    case MList(items: List[MetaValue])
    case MRecord(fields: Map[String, MetaValue])
    case MVariant(tag: String, value: MetaValue)
    case MLambda(param: String, body: MetaTerm)
    case MRef(name: String)

  // ===========================================================================
  // Meta-Grammar: Phi Grammar represented as Phi Terms
  // ===========================================================================

  /**
   * Phi's grammar expressed as a Phi term structure.
   */
  val phiGrammarSpec: SpecFile = SpecFile(
    name = "Phi",
    sections = List(
      Section.Grammar(GrammarSec(
        name = "Core",
        rules = List(
          // Term = Done value | Hole label?
          GrammarRule(
            name = "Term",
            typeRef = Some(TypeRef.Parameterized("Term", List(TypeRef.Simple("A")))),
            alternatives = List(
              Alternative(
                parts = List(
                  GrammarPart.Terminal("Done"),
                  GrammarPart.NonTerminal("value", Some("v"))
                ),
                action = Some("Term.Done(v)"),
                label = Some("done")
              ),
              Alternative(
                parts = List(
                  GrammarPart.Terminal("Hole"),
                  GrammarPart.Optional(GrammarPart.NonTerminal("label"))
                ),
                action = Some("Term.Hole(label)"),
                label = Some("hole")
              )
            ),
            annotations = List(Annotation("sealed"))
          ),

          // Syntax combinators
          GrammarRule(
            name = "Syntax",
            typeRef = Some(TypeRef.Parameterized("Syntax", List(TypeRef.Simple("A")))),
            alternatives = List(
              Alternative(
                parts = List(GrammarPart.NonTerminal("keyword")),
                action = Some("Syntax.keyword"),
                label = Some("kw")
              ),
              Alternative(
                parts = List(GrammarPart.NonTerminal("symbol")),
                action = Some("Syntax.symbol"),
                label = Some("sym")
              ),
              Alternative(
                parts = List(
                  GrammarPart.NonTerminal("left"),
                  GrammarPart.Terminal("~"),
                  GrammarPart.NonTerminal("right")
                ),
                action = Some("left ~ right"),
                label = Some("seq")
              ),
              Alternative(
                parts = List(
                  GrammarPart.NonTerminal("first"),
                  GrammarPart.Terminal("|"),
                  GrammarPart.NonTerminal("second")
                ),
                action = Some("first | second"),
                label = Some("choice")
              )
            )
          ),

          // Change operations
          GrammarRule(
            name = "Change",
            typeRef = Some(TypeRef.Parameterized("Change", List(TypeRef.Simple("A")))),
            alternatives = List(
              Alternative(
                parts = List(GrammarPart.Terminal("Insert"), GrammarPart.NonTerminal("value")),
                action = Some("Change.insert(value)")
              ),
              Alternative(
                parts = List(GrammarPart.Terminal("Replace"), GrammarPart.NonTerminal("term")),
                action = Some("Change.replace(term)")
              ),
              Alternative(
                parts = List(GrammarPart.Terminal("Delete")),
                action = Some("Change.delete")
              )
            )
          ),

          // Xform
          GrammarRule(
            name = "Xform",
            typeRef = Some(TypeRef.Parameterized("Xform", List(TypeRef.Simple("A"), TypeRef.Simple("B")))),
            alternatives = List(
              Alternative(
                parts = List(
                  GrammarPart.Terminal("xform"),
                  GrammarPart.NonTerminal("forward"),
                  GrammarPart.NonTerminal("backward")
                ),
                action = Some("Xform(forward, backward)")
              )
            )
          )
        )
      )),

      Section.Grammar(GrammarSec(
        name = "LC",
        rules = List(
          GrammarRule(
            name = "Expr",
            typeRef = Some(TypeRef.Simple("LC")),
            alternatives = List(
              Alternative(
                parts = List(GrammarPart.NonTerminal("ident")),
                action = Some("LC.Var"),
                label = Some("var")
              ),
              Alternative(
                parts = List(
                  GrammarPart.Terminal("fn"),
                  GrammarPart.NonTerminal("param"),
                  GrammarPart.Terminal("=>"),
                  GrammarPart.NonTerminal("body")
                ),
                action = Some("LC.Lam"),
                label = Some("lam")
              ),
              Alternative(
                parts = List(
                  GrammarPart.NonTerminal("func"),
                  GrammarPart.NonTerminal("arg")
                ),
                action = Some("LC.App"),
                label = Some("app")
              ),
              Alternative(
                parts = List(GrammarPart.NonTerminal("intLit")),
                action = Some("LC.Lit"),
                label = Some("lit")
              )
            )
          )
        )
      )),

      Section.Config(ConfigSec(Map(
        "version" -> "0.1.0",
        "target" -> "scala3"
      )))
    )
  )

  // ===========================================================================
  // Meta-Repository: Populated with example terms
  // ===========================================================================

  /**
   * Create a repository populated with Phi meta-language examples.
   */
  def createMetaRepo(): Repo[LC] =
    val repo = Repo[LC]()

    // Identity function: λx.x
    val identity = LC.Lam("x", LC.Var("x"))
    repo.store(Term.Done(identity), Set(Name("std.identity")))

    // Constant function: λx.λy.x
    val const = LC.Lam("x", LC.Lam("y", LC.Var("x")))
    repo.store(Term.Done(const), Set(Name("std.const")))

    // Compose: λf.λg.λx.f(g(x))
    val compose = LC.Lam("f", LC.Lam("g", LC.Lam("x",
      LC.App(LC.Var("f"), LC.App(LC.Var("g"), LC.Var("x")))
    )))
    repo.store(Term.Done(compose), Set(Name("std.compose")))

    // Church numerals
    val zero = LC.Lam("f", LC.Lam("x", LC.Var("x")))
    repo.store(Term.Done(zero), Set(Name("church.zero")))

    val succ = LC.Lam("n", LC.Lam("f", LC.Lam("x",
      LC.App(LC.Var("f"), LC.App(LC.App(LC.Var("n"), LC.Var("f")), LC.Var("x")))
    )))
    repo.store(Term.Done(succ), Set(Name("church.succ")))

    // Boolean combinators
    val trueLC = LC.Lam("t", LC.Lam("f", LC.Var("t")))
    repo.store(Term.Done(trueLC), Set(Name("bool.true")))

    val falseLC = LC.Lam("t", LC.Lam("f", LC.Var("f")))
    repo.store(Term.Done(falseLC), Set(Name("bool.false")))

    val ifThenElse = LC.Lam("b", LC.Lam("t", LC.Lam("f",
      LC.App(LC.App(LC.Var("b"), LC.Var("t")), LC.Var("f"))
    )))
    repo.store(Term.Done(ifThenElse), Set(Name("bool.if")))

    // Arithmetic primitives
    val add = LC.Prim("+", List(LC.Var("x"), LC.Var("y")))
    repo.store(Term.Done(LC.Lam("x", LC.Lam("y", add))), Set(Name("arith.add")))

    val mul = LC.Prim("*", List(LC.Var("x"), LC.Var("y")))
    repo.store(Term.Done(LC.Lam("x", LC.Lam("y", mul))), Set(Name("arith.mul")))

    repo

  // ===========================================================================
  // Meta-Xforms: Transformations as Terms
  // ===========================================================================

  /**
   * Representation of Xforms as data.
   */
  case class XformDef(
    name: String,
    sourceType: String,
    targetType: String,
    description: String,
    isReversible: Boolean
  )

  val registeredXforms: List[XformDef] = List(
    XformDef("lc-to-ic", "LC", "ICNet", "Compile Lambda Calculus to Interaction Nets", true),
    XformDef("typecheck", "LC", "TypedLC", "Bidirectional type inference", true),
    XformDef("pattern-compile", "PatternMatch", "LC", "Compile pattern matches to LC", true),
    XformDef("cps-transform", "LC", "LC", "Continuation-passing style transform", true),
    XformDef("anf-transform", "LC", "LC", "A-normal form transform", true)
  )

  // ===========================================================================
  // Meta-Editor: Structured editing as Terms
  // ===========================================================================

  /**
   * Editor action represented as a term.
   */
  enum EditorAction:
    case Navigate(direction: String)  // "up", "down", "left", "right"
    case Fill(value: MetaValue)
    case Delete
    case Replace(term: MetaTerm)
    case Undo
    case Redo
    case Transform(xformName: String)

  /**
   * Editor state as a term.
   */
  case class EditorStateMeta(
    currentPath: List[Int],  // Path to current focus
    term: MetaTerm,
    history: List[MetaTerm],
    redoStack: List[MetaTerm]
  )

  // ===========================================================================
  // Phi Bootstrapping Example
  // ===========================================================================

  /**
   * Demonstrates Phi parsing Phi grammar, then using that to parse code.
   */
  def bootstrapExample(): Unit =
    // 1. Parse a simple expression using the expression parser
    val exprInput = "let double = fn x => x * 2 in double 5"
    val tokens = Lexer.tokenize(exprInput)
    val exprResult = ExprParser.expr().parse(tokens)

    println(s"Parsed expression: ${exprResult.term}")

    // 2. Store in repo
    val repo = createMetaRepo()

    // 3. Type check some stored terms
    repo.getByName(Name("std.identity")).foreach { term =>
      val typed = TypeChecker.forward(term)
      println(s"std.identity type: ${typed.toOption.map(_.getType.render)}")
    }

    // 4. Transform to IC
    repo.getByName(Name("std.compose")).foreach { term =>
      val ic = LCToIC.forward(term)
      println(s"std.compose IC nodes: ${ic.toOption.map(_.nodes.size)}")
    }

    // 5. Demonstrate edit → transform pipeline
    val editor = Editor(Term.Done(LC.Lit(5)))
    val edited = editor.applyChange(Change.Map[LC, LC](
      { case LC.Lit(n) => LC.Lam("x", LC.Lit(n)); case x => x },
      { case LC.Lam(_, LC.Lit(n)) => LC.Lit(n); case x => x.asInstanceOf[LC] }
    ))
    println(s"Edited term: ${edited.current}")

    val typedEdited = TypeChecker.forward(edited.current)
    println(s"Edited type: ${typedEdited.toOption.map(_.getType.render)}")

  // ===========================================================================
  // Meta-Language Expressions
  // ===========================================================================

  /**
   * Convert Expr to LC for meta-level manipulation.
   */
  def exprToLC(expr: Expr): LC = expr match
    case Expr.Num(n)           => LC.Lit(n)
    case Expr.Var(name)        => LC.Var(name)
    case Expr.BinOp(op, l, r)  => LC.Prim(op, List(exprToLC(l), exprToLC(r)))
    case Expr.UnaryOp(op, e)   => LC.Prim(op, List(exprToLC(e)))
    case Expr.Paren(inner)     => exprToLC(inner)
    case Expr.Call(f, args)    => args.foldLeft(LC.Var(f): LC)((acc, arg) => LC.App(acc, exprToLC(arg)))
    case Expr.IfExpr(c, t, e)  => LC.Prim("if", List(exprToLC(c), exprToLC(t), exprToLC(e)))
    case Expr.LetExpr(n, v, b) => LC.Let(n, exprToLC(v), exprToLC(b))

  /**
   * Convert LC to Expr where possible.
   */
  def lcToExpr(lc: LC): Option[Expr] = lc match
    case LC.Lit(n)  => Some(Expr.Num(n))
    case LC.Var(n)  => Some(Expr.Var(n))
    case LC.Prim(op, List(l, r)) if Expr.allOps.exists(_.symbol == op) =>
      for
        le <- lcToExpr(l)
        re <- lcToExpr(r)
      yield Expr.BinOp(op, le, re)
    case LC.Let(n, v, b) =>
      for
        ve <- lcToExpr(v)
        be <- lcToExpr(b)
      yield Expr.LetExpr(n, ve, be)
    case _ => None

  /** Xform between Expr and LC */
  val exprToLCXform: Xform[Expr, LC] = Xform(
    fwd = _.map(exprToLC),
    bwd = term => term.flatMap(lc => lcToExpr(lc).map(Term.Done(_)).getOrElse(Term.Hole(Some("cannot convert"))))
  )

  // ===========================================================================
  // Phi Grammar as Phi Term
  // ===========================================================================

  /**
   * Store the Phi grammar spec itself in a repository.
   */
  def storeGrammarInRepo(): Repo[SpecFile] =
    val repo = Repo[SpecFile]()
    repo.store(Term.Done(phiGrammarSpec), Set(Name("phi.grammar")))
    repo

  /**
   * Demonstrate grammar round-trip.
   */
  def grammarRoundTrip(): Boolean =
    val repo = storeGrammarInRepo()
    val retrieved = repo.getByName(Name("phi.grammar"))

    retrieved match
      case Some(Term.Done(spec)) =>
        spec.name == "Phi" &&
        spec.sections.size == 3 &&
        spec.sections.head.isInstanceOf[Section.Grammar]
      case _ => false

  // ===========================================================================
  // Example Programs
  // ===========================================================================

  /** Example: Factorial in LC */
  val factorial: LC =
    // Y combinator: λf.(λx.f(x x))(λx.f(x x))
    val y = LC.Lam("f",
      LC.App(
        LC.Lam("x", LC.App(LC.Var("f"), LC.App(LC.Var("x"), LC.Var("x")))),
        LC.Lam("x", LC.App(LC.Var("f"), LC.App(LC.Var("x"), LC.Var("x"))))
      )
    )

    // λfact.λn.if n==0 then 1 else n * fact(n-1)
    val factBody = LC.Lam("fact", LC.Lam("n",
      LC.Prim("if", List(
        LC.Prim("==", List(LC.Var("n"), LC.Lit(0))),
        LC.Lit(1),
        LC.Prim("*", List(
          LC.Var("n"),
          LC.App(LC.Var("fact"), LC.Prim("-", List(LC.Var("n"), LC.Lit(1))))
        ))
      ))
    ))

    LC.App(y, factBody)

  /** Example: Map function in LC */
  val mapLC: LC =
    // λmap.λf.λlist.if null(list) then nil else cons(f(head list), map f (tail list))
    LC.Lam("map", LC.Lam("f", LC.Lam("list",
      LC.Prim("if", List(
        LC.App(LC.Var("null"), LC.Var("list")),
        LC.Var("nil"),
        LC.Prim("cons", List(
          LC.App(LC.Var("f"), LC.App(LC.Var("head"), LC.Var("list"))),
          LC.App(LC.App(LC.Var("map"), LC.Var("f")), LC.App(LC.Var("tail"), LC.Var("list")))
        ))
      ))
    )))
