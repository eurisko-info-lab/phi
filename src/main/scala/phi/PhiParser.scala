package phi

import scala.util.parsing.combinator.*

/**
 * Parser for the Phi language definition syntax.
 * 
 * Parses specs like:
 * 
 *   language Phi {
 *     sort Type
 *     sort Term
 *     
 *     constructor Unit : Type
 *     constructor succ : Term → Term
 *     
 *     rule BetaReduce {
 *       app (lam x A body) v ↦ body[x := v]
 *     }
 *     
 *     def add : Term = lam m Nat (...)
 *     
 *     normalize := repeat (BetaReduce | NatRecZero)
 *   }
 */
object PhiParser extends RegexParsers:
  
  // Skip whitespace and comments
  override val whiteSpace = """(\s|//[^\n]*)+""".r
  
  // =========================================================================
  // Lexical Elements
  // =========================================================================
  
  def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
  
  def number: Parser[Int] = """\d+""".r ^^ (_.toInt)
  
  // Keywords
  def LANGUAGE = "language"
  def SORT = "sort"
  def CONSTRUCTOR = "constructor"
  def XFORM = "xform"
  def CHANGE = "change"
  def RULE = "rule"
  def DEF = "def"
  def WHERE = "where"
  def AND = "and"
  
  // Symbols - support both Unicode and ASCII
  def ARROW = "→" | "->"
  def BIARROW = "⇄" | "<->"
  def MAPSTO = "↦" | "|->"
  def LAMBDA = "λ" | "\\"
  def ASSIGN = ":=" | "="
  
  // =========================================================================
  // Top Level
  // =========================================================================
  
  def spec: Parser[LangSpec] =
    LANGUAGE ~> ident ~ ("{" ~> rep(declaration) <~ "}") ^^ {
      case name ~ decls =>
        val sorts = decls.collect { case s: Sort => s }
        val cons = decls.collect { case c: Constructor => c }
        val xforms = decls.collect { case x: XformSpec => x }
        val changes = decls.collect { case c: ChangeSpec => c }
        val rules = decls.collect { case r: Rule => r }
        val defs = decls.collect { case d: Def => d }
        val strats = decls.collect { case s: (String, Strat) @unchecked => s }.toMap
        LangSpec(name, sorts, cons, xforms, changes, rules, defs, strats)
    }
  
  def declaration: Parser[Any] =
    sortDecl | constructorDecl | xformDecl | changeDecl | ruleDecl | defDecl | strategyDecl
  
  // =========================================================================
  // Declarations
  // =========================================================================
  
  // sort Type
  def sortDecl: Parser[Sort] =
    SORT ~> ident ^^ Sort.apply
  
  // constructor Unit : Type
  // constructor succ : Term → Term
  // constructor lam : (x : Term) → Type → Term → Term
  def constructorDecl: Parser[Constructor] =
    CONSTRUCTOR ~> ident ~ (":" ~> constructorType) ^^ {
      case name ~ ((params, ret)) => Constructor(name, params, ret)
    }
  
  def constructorType: Parser[(List[(Option[String], LangType)], String)] =
    rep(paramType <~ ARROW) ~ ident ^^ {
      case params ~ ret => (params, ret)
    }
  
  def paramType: Parser[(Option[String], LangType)] =
    // (x : Term) or just Term
    ("(" ~> ident ~ (":" ~> langType) <~ ")") ^^ { case n ~ t => (Some(n), t) } |
    langType ^^ { t => (None, t) }
  
  def langType: Parser[LangType] =
    ident ^^ LangType.SortRef.apply
  
  // xform TypeCheck : Term ⇄ Type  
  def xformDecl: Parser[XformSpec] =
    XFORM ~> ident ~ (":" ~> ident) ~ (BIARROW ~> ident) ^^ {
      case name ~ src ~ tgt => XformSpec(name, src, tgt)
    }
  
  // change BetaReduce : Term ⇄ Term
  def changeDecl: Parser[ChangeSpec] =
    CHANGE ~> ident ~ (":" ~> ident <~ BIARROW <~ ident) ^^ {
      case name ~ sort => ChangeSpec(name, sort)
    }
  
  // rule BetaReduce { pattern ↦ result }
  def ruleDecl: Parser[Rule] =
    RULE ~> ident ~ ("{" ~> rep(ruleCase) <~ "}") ^^ {
      case name ~ cases => Rule(name, RuleDir.Both, cases)
    } |
    // Single-line rule: rule BetaReduce { lhs ↦ rhs }
    RULE ~> ident ~ ("{" ~> ruleCase <~ "}") ^^ {
      case name ~ cas => Rule(name, RuleDir.Both, List(cas))
    }
  
  def ruleCase: Parser[RuleCase] =
    pattern ~ (MAPSTO ~> pattern) ~ opt(guards) ^^ {
      case lhs ~ rhs ~ gs => RuleCase(lhs, rhs, gs.getOrElse(Nil))
    }
  
  def guards: Parser[List[RuleGuard]] =
    WHERE ~> repsep(guard, AND)
  
  def guard: Parser[RuleGuard] =
    ident ~ ("=" ~> pattern) ^^ {
      case v ~ expected => RuleGuard(v, Pat.PVar(v), expected)
    }
  
  // def add : Term = lam m Nat (...)
  def defDecl: Parser[Def] =
    DEF ~> ident ~ opt(":" ~> ident) ~ (ASSIGN ~> pattern) ^^ {
      case name ~ sort ~ body => Def(name, sort, body)
    }
  
  // normalize := repeat (BetaReduce | NatRecZero | NatRecSucc)
  def strategyDecl: Parser[(String, Strat)] =
    ident ~ (ASSIGN ~> strategy) ^^ { case n ~ s => (n, s) }
  
  // =========================================================================
  // Patterns
  // =========================================================================
  
  def pattern: Parser[Pat] = substitution | application
  
  // body[x := v]
  def substitution: Parser[Pat] =
    atomPattern ~ ("[" ~> ident ~ (ASSIGN ~> pattern) <~ "]") ^^ {
      case body ~ (varName ~ repl) => Pat.PSubst(body, varName, repl)
    }
  
  // f a b c = app(app(app(f, a), b), c)
  def application: Parser[Pat] =
    rep1(atomPattern) ^^ {
      case List(single) => single
      case head :: tail => tail.foldLeft(head)((f, a) => Pat.PApp(f, a))
      case Nil => ??? // unreachable with rep1
    }
  
  def atomPattern: Parser[Pat] =
    "(" ~> pattern <~ ")" |
    number ^^ { n => numeral(n) } |
    LAMBDA ~> ident ~ ("." ~> pattern) ^^ { case p ~ b => 
      Pat.PCon("lam", List(Pat.PCon(p, Nil), Pat.PVar("_"), b)) 
    } |
    ident ^^ { name => 
      // In rule patterns, identifiers are metavariables unless they're known constructors
      // This includes both lowercase (x, body) and uppercase (A, T) metavars
      if isKnownConstructor(name) then Pat.PCon(name, Nil)
      else Pat.PVar(name)
    }
  
  // Known constructors that start lowercase
  def isKnownConstructor(name: String): Boolean =
    Set("zero", "succ", "unit", "lam", "app", "pair", "fst", "snd", 
        "NatRec", "true", "false").contains(name)
  
  // Build numeral as succ^n(zero)
  def numeral(n: Int): Pat =
    if n == 0 then Pat.PCon("zero", Nil)
    else Pat.PCon("succ", List(numeral(n - 1)))
  
  // =========================================================================
  // Strategies
  // =========================================================================
  
  def strategy: Parser[Strat] = strategyChoice
  
  def strategyChoice: Parser[Strat] =
    strategySeq ~ rep(("|" | "∣") ~> strategySeq) ^^ {
      case first ~ rest => rest.foldLeft(first)(Strat.Choice.apply)
    }
  
  def strategySeq: Parser[Strat] =
    strategyAtom ~ rep((";" | "⨟") ~> strategyAtom) ^^ {
      case first ~ rest => rest.foldLeft(first)(Strat.Seq.apply)
    }
  
  def strategyAtom: Parser[Strat] =
    "(" ~> strategy <~ ")" |
    "repeat" ~> strategyAtom ^^ Strat.Repeat.apply |
    "id" ^^^ Strat.Id |
    ident ^^ Strat.Apply.apply
  
  // =========================================================================
  // Entry Point
  // =========================================================================
  
  def parse(input: String): Either[String, LangSpec] =
    parseAll(spec, input) match
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)


// =============================================================================
// Extended Parser with Full Pattern Support
// =============================================================================

/**
 * Full Phi parser supporting constructor application patterns.
 * 
 * Pattern syntax:
 *   app (lam x A body) v     - constructor with args
 *   body[x := v]             - substitution
 *   λx.body                  - lambda sugar
 *   42                       - numeral sugar
 */
object PhiParserFull extends RegexParsers:
  
  override val whiteSpace = """(\s|//[^\n]*)+""".r
  
  def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
  def number: Parser[Int] = """\d+""".r ^^ (_.toInt)
  
  def LANGUAGE = "language"
  def SORT = "sort" 
  def CONSTRUCTOR = "constructor"
  def XFORM = "xform"
  def CHANGE = "change"
  def RULE = "rule"
  def DEF = "def"
  
  val keywords = Set("language", "sort", "constructor", "xform", "change", "rule", "def", "repeat", "id")
  
  def ARROW = "→" | "->"
  def BIARROW = "⇄" | "<->"
  def MAPSTO = "↦" | "|->"
  def LAMBDA = "λ" | "\\"
  def ASSIGN = ":="
  def EQUALS = "="
  
  // =========================================================================
  // Top Level
  // =========================================================================
  
  def spec: Parser[LangSpec] =
    LANGUAGE ~> ident ~ ("{" ~> rep(declaration) <~ "}") ^^ {
      case name ~ decls =>
        LangSpec(
          name,
          decls.collect { case s: Sort => s },
          decls.collect { case c: Constructor => c },
          decls.collect { case x: XformSpec => x },
          decls.collect { case c: ChangeSpec => c },
          decls.collect { case r: Rule => r },
          decls.collect { case d: Def => d },
          decls.collect { case (n: String, s: Strat) => (n, s) }.toMap
        )
    }
  
  def declaration: Parser[Any] =
    sortDecl | constructorDecl | xformDecl | changeDecl | ruleDecl | defDecl | strategyDecl
  
  def sortDecl: Parser[Sort] = SORT ~> ident ^^ Sort.apply
  
  def constructorDecl: Parser[Constructor] =
    CONSTRUCTOR ~> ident ~ (":" ~> rep(typeArg <~ ARROW) ~ ident) ^^ {
      case name ~ (params ~ ret) => Constructor(name, params, ret)
    }
  
  def typeArg: Parser[(Option[String], LangType)] =
    "(" ~> ident ~ (":" ~> ident) <~ ")" ^^ { case n ~ t => (Some(n), LangType.SortRef(t)) } |
    ident ^^ { t => (None, LangType.SortRef(t)) }
  
  def xformDecl: Parser[XformSpec] =
    XFORM ~> ident ~ (":" ~> ident) ~ (BIARROW ~> ident) ^^ {
      case name ~ src ~ tgt => XformSpec(name, src, tgt)
    }
  
  def changeDecl: Parser[ChangeSpec] =
    CHANGE ~> ident ~ (":" ~> ident <~ BIARROW <~ ident) ^^ {
      case name ~ sort => ChangeSpec(name, sort)
    }
  
  def ruleDecl: Parser[Rule] =
    RULE ~> ident ~ ("{" ~> rep1(ruleCase) <~ "}") ^^ {
      case name ~ cases => Rule(name, RuleDir.Both, cases)
    }
  
  def ruleCase: Parser[RuleCase] =
    pattern ~ (MAPSTO ~> pattern) ^^ {
      case lhs ~ rhs => RuleCase(lhs, rhs, Nil)
    }
  
  def defDecl: Parser[Def] =
    DEF ~> ident ~ opt(":" ~> ident) ~ (EQUALS ~> termPattern) ^^ {
      case name ~ sort ~ body => Def(name, sort, body)
    }
  
  // Term patterns for definitions - all idents become constructors (no metavars)
  def termPattern: Parser[Pat] = termSubst
  
  def termSubst: Parser[Pat] =
    termApp ~ opt("[" ~> ident ~ (ASSIGN ~> termPattern) <~ "]") ^^ {
      case body ~ Some(v ~ repl) => Pat.PSubst(body, v, repl)
      case body ~ None => body
    }
  
  def termApp: Parser[Pat] =
    rep1(termAtom) ^^ { atoms =>
      atoms.reduceLeft((f, a) => Pat.PCon("app", List(f, a)))
    }
  
  def termAtom: Parser[Pat] =
    "(" ~> termPattern <~ ")" |
    number ^^ numeral |
    LAMBDA ~> rep1(nonKeywordIdent) ~ ("." ~> termPattern) ^^ { case params ~ body =>
      params.foldRight(body)((p, b) => Pat.PCon("lam", List(Pat.PCon(p, Nil), Pat.PVar("_"), b)))
    } |
    termConstructorPat |
    nonKeywordIdent ^^ { name => Pat.PCon(name, Nil) }  // Always constructor, never metavar
  
  def termConstructorPat: Parser[Pat] =
    ident ~ ("(" ~> repsep(termPattern, ",") <~ ")") ^^ {
      case name ~ args => Pat.PCon(name, args)
    }
  
  def strategyDecl: Parser[(String, Strat)] =
    ident ~ (ASSIGN ~> strategy) ^^ { case n ~ s => (n, s) }
  
  // =========================================================================
  // Patterns - Key difference: constructor application
  // =========================================================================
  
  def pattern: Parser[Pat] = substitutionPat
  
  def substitutionPat: Parser[Pat] =
    applicationPat ~ opt("[" ~> ident ~ (ASSIGN ~> pattern) <~ "]") ^^ {
      case body ~ Some(v ~ repl) => Pat.PSubst(body, v, repl)
      case body ~ None => body
    }
  
  // Application is left-associative: f a b = (f a) b
  def applicationPat: Parser[Pat] =
    rep1(atomPat) ^^ { atoms =>
      atoms.reduceLeft((f, a) => Pat.PCon("app", List(f, a)))
    }
  
  def atomPat: Parser[Pat] =
    "(" ~> pattern <~ ")" |
    number ^^ numeral |
    LAMBDA ~> rep1(nonKeywordIdent) ~ ("." ~> pattern) ^^ { case params ~ body =>
      params.foldRight(body)((p, b) => Pat.PCon("lam", List(Pat.PCon(p, Nil), Pat.PVar("_"), b)))
    } |
    constructorPat |
    nonKeywordIdent ^^ { name =>
      // In rule patterns, identifiers are metavariables unless they're known constructors
      if knownCons(name) then Pat.PCon(name, Nil)
      else Pat.PVar(name)
    }
  
  // Identifier that's not a keyword
  def nonKeywordIdent: Parser[String] = 
    ident.filter(s => !keywords.contains(s))
  
  // Constructor with explicit args: Con(a, b, c) or Con a b c when in parens
  def constructorPat: Parser[Pat] =
    ident ~ ("(" ~> repsep(pattern, ",") <~ ")") ^^ {
      case name ~ args => Pat.PCon(name, args)
    }
  
  def knownCons(s: String) = Set("zero", "succ", "unit", "lam", "app", 
    "pair", "fst", "snd", "NatRec", "true", "false").contains(s)
  
  def numeral(n: Int): Pat =
    if n == 0 then Pat.PCon("zero", Nil)
    else Pat.PCon("succ", List(numeral(n - 1)))
  
  // =========================================================================
  // Strategies
  // =========================================================================
  
  def strategy: Parser[Strat] =
    strategySeq ~ rep("|" ~> strategySeq) ^^ {
      case first ~ rest => rest.foldLeft(first)(Strat.Choice.apply)
    }
  
  def strategySeq: Parser[Strat] =
    strategyAtom ~ rep(";" ~> strategyAtom) ^^ {
      case first ~ rest => rest.foldLeft(first)(Strat.Seq.apply)
    }
  
  def strategyAtom: Parser[Strat] =
    "(" ~> strategy <~ ")" |
    "repeat" ~> strategyAtom ^^ Strat.Repeat.apply |
    "id" ^^^ Strat.Id |
    ident ^^ Strat.Apply.apply
  
  def parse(input: String): Either[String, LangSpec] =
    parseAll(spec, input) match
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(s"${failure.msg} at ${failure.next.pos}")
