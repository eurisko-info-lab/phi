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
 * 
 * Pattern syntax:
 *   app (lam x A body) v     - constructor with args
 *   body[x := v]             - substitution
 *   λx.body                  - lambda sugar
 *   42                       - numeral sugar
 *   (a, b, c)                - tuple pattern
 *   [h | t]                  - list cons pattern
 *   []                       - empty list
 *   Xform.forward(args)      - qualified xform call
 */
object PhiParser extends RegexParsers:
  
  // Whitespace includes line comments (//) and block comments (/* */)
  override val whiteSpace = """(\s|//[^\n]*|/\*[\s\S]*?\*/)+""".r
  
  // Support Unicode in identifiers (λProlog, etc.)
  def ident: Parser[String] = """[\p{L}_][\p{L}\p{N}_]*""".r
  def qualifiedIdent: Parser[String] = """[\p{L}_][\p{L}\p{N}_]*(?:\.[\p{L}_][\p{L}\p{N}_]*)*""".r
  def number: Parser[Int] = """\d+""".r ^^ (_.toInt)
  def stringLit: Parser[String] = """"[^"]*"""".r ^^ { s => s.substring(1, s.length - 1) }
  
  def LANGUAGE = "language"
  def SORT = "sort" 
  def CONSTRUCTOR = "constructor"
  def XFORM = "xform"
  def CHANGE = "change"
  def RULE = "rule"
  def DEF = "def"
  def WHERE = "where"
  def AND = "and"
  def TOKEN = "token"
  def SYNTAX = "syntax"
  def GRAMMAR = "grammar"
  def PARSE = "parse"
  def STRATEGY = "strategy"
  def THEOREM = "theorem"
  
  val keywords = Set("language", "sort", "constructor", "xform", "change", "rule", "def", 
                     "repeat", "id", "where", "and", "token", "syntax", "grammar", "parse", "strategy", "theorem")
  
  def ARROW: Parser[String] = "→" | "->"
  def BIARROW: Parser[String] = "⇄" | "<->"
  def MAPSTO: Parser[String] = "↦" | "|->"
  def LAMBDA: Parser[String] = "λ" | "\\"
  def ASSIGN: Parser[String] = ":="
  def EQUALS: Parser[String] = "="
  def TIMES: Parser[String] = "×" | "*"
  def NEQ: Parser[String] = "≠" | "!="
  def CONS: Parser[String] = "::" | "|"  // | is safe inside [...] brackets
  
  // =========================================================================
  // Top Level
  // =========================================================================
  
  def spec: Parser[LangSpec] =
    LANGUAGE ~> ident ~ ("{" ~> rep(declaration) <~ "}") ^^ {
      case name ~ decls =>
        val flat = decls.flatten
        LangSpec(
          name,
          flat.collect { case s: Sort => s },
          flat.collect { case c: Constructor => c },
          flat.collect { case x: XformSpec => x },
          flat.collect { case c: ChangeSpec => c },
          flat.collect { case r: Rule => r },
          flat.collect { case d: Def => d },
          flat.collect { case (n: String, s: Strat) => (n, s) }.toMap,
          flat.collect { case t: Theorem => t }
        )
    }
  
  def declaration: Parser[List[Any]] =
    sortDecl ^^ (List(_)) | 
    constructorBlock | 
    xformDecl ^^ (List(_)) | 
    changeDecl ^^ (List(_)) | 
    ruleDecl ^^ (List(_)) | 
    theoremDecl ^^ (List(_)) |     // Theorem declarations
    strategyDecl ^^ (List(_)) |  // Before defDecl - both start with ident
    defDecl ^^ (List(_)) | 
    tokenBlock ^^ (_ => Nil) |     // Parse but ignore token blocks for now
    syntaxDecl ^^ (_ => Nil) |     // Parse but ignore syntax decls
    grammarBlock ^^ (_ => Nil)     // Parse but ignore grammar blocks
  
  def sortDecl: Parser[Sort] = SORT ~> ident ^^ Sort.apply
  
  // Constructor declaration: single line only
  // constructor Foo : A -> B
  // For multi-line in source files, just repeat the keyword on each line
  def constructorBlock: Parser[List[Constructor]] =
    CONSTRUCTOR ~> constructorLine ^^ (List(_))
  
  def constructorLine: Parser[Constructor] =
    ident ~ (":" ~> constructorType) ^^ {
      case name ~ ((params, ret)) => Constructor(name, params, ret)
    }
  
  def constructorType: Parser[(List[(Option[String], LangType)], String)] =
    rep(typeArg <~ ARROW) ~ ident ^^ { case params ~ ret => (params, ret) }
  
  // Type arg in constructor signature - non-arrow to avoid greedy consumption
  def typeArg: Parser[(Option[String], LangType)] =
    "(" ~> ident ~ (":" ~> typeExpr) <~ ")" ^^ { case n ~ t => (Some(n), t) } |
    "(" ~> typeExpr <~ ")" ^^ { t => (None, t) } |  // Parenthesized arrow types OK
    productType ^^ { t => (None, t) }  // Non-parenthesized must be non-arrow
  
  // Type expressions: A, A → B, A × B, A*, (A × B) → C
  // Arrow is right-associative: A → B → C = A → (B → C)
  def typeExpr: Parser[LangType] = arrowType
  
  def arrowType: Parser[LangType] =
    productType ~ opt(ARROW ~> arrowType) ^^ {
      case a ~ Some(b) => LangType.Arrow(a, b)
      case a ~ None => a
    }
  
  def productType: Parser[LangType] =
    listType ~ rep(TIMES ~> listType) ^^ {
      case first ~ rest => rest.foldLeft(first)(LangType.Product.apply)
    }
  
  def listType: Parser[LangType] =
    atomType ~ opt("*") ^^ {
      case t ~ Some(_) => LangType.ListOf(t)
      case t ~ None => t
    }
    
  def atomType: Parser[LangType] =
    "(" ~> typeExpr <~ ")" |
    ident ^^ LangType.SortRef.apply
  
  def xformDecl: Parser[XformSpec] =
    XFORM ~> ident ~ (":" ~> typeExpr) ~ (BIARROW ~> typeExpr) ^^ {
      case name ~ src ~ tgt => XformSpec(name, typeToString(src), typeToString(tgt))
    }
  
  def changeDecl: Parser[ChangeSpec] =
    CHANGE ~> ident ~ (":" ~> ident <~ BIARROW <~ ident) ^^ {
      case name ~ sort => ChangeSpec(name, sort)
    }
  
  // Theorem declaration: theorem Name : Type
  // Like Abella, this declares a proposition to prove
  def theoremDecl: Parser[Theorem] =
    THEOREM ~> ident ~ (":" ~> typeExpr) ^^ {
      case name ~ sig => Theorem(name, sig, None)
    }
  
  // Rules can have qualified names: rule Subst.forward { ... }
  def ruleDecl: Parser[Rule] =
    RULE ~> qualifiedIdent ~ ("{" ~> rep1(ruleCase) <~ "}") ^^ {
      case name ~ cases => Rule(name, RuleDir.Both, cases)
    }
  
  def ruleCase: Parser[RuleCase] =
    pattern ~ (MAPSTO ~> ruleRhs) ~ opt(whereClause) ^^ {
      case lhs ~ rhs ~ guards => RuleCase(lhs, rhs, guards.getOrElse(Nil))
    }
  
  // RHS of a rule - single atom, or constructor with parens args, or application of atoms
  // Also handles ++ (list concat) and other binary ops
  def ruleRhs: Parser[Pat] =
    rhsConcat
  
  def rhsConcat: Parser[Pat] =
    rhsApp ~ opt("++" ~> rhsApp) ^^ {
      case l ~ Some(r) => Pat.PCon("concat", List(l, r))
      case l ~ None => l
    }
  
  def rhsApp: Parser[Pat] =
    constructorPat |  // Con(a, b, c) - explicit args in parens
    qualifiedPat |    // Xform.forward(args)
    rep1(rhsAtom) ~ opt("[" ~> ident ~ (ASSIGN ~> pattern) <~ "]") ^^ {
      case atoms ~ None => 
        atoms.reduceLeft((f, a) => Pat.PCon("app", List(f, a)))
      case atoms ~ Some(v ~ repl) =>
        val base = atoms.reduceLeft((f, a) => Pat.PCon("app", List(f, a)))
        Pat.PSubst(base, v, repl)
    }
  
  // Atom allowed in RHS - includes parenthesized expressions that aren't tuple LHS patterns
  // A tuple LHS would be (a, b, c), Foo... - with comma at end
  // A function arg would be (Subst.forward(...)) - application inside parens
  def rhsAtom: Parser[Pat] =
    listPat |
    number ^^ numeral |
    constructorPat |
    qualifiedPat |
    // Parenthesized application (not tuple-like) - starts with uppercase or qualified
    "(" ~> applicationPat <~ ")" |
    nonKeywordIdent ^^ { name =>
      if knownCons(name) then Pat.PCon(name, Nil)
      else Pat.PVar(name)
    }
  
  // Atom that's not a parenthesized expression (for guards)
  def nonParenAtom: Parser[Pat] =
    listPat |
    number ^^ numeral |
    constructorPat |
    qualifiedPat |
    nonKeywordIdent ^^ { name =>
      if knownCons(name) then Pat.PCon(name, Nil)
      else Pat.PVar(name)
    }
  
  // where x = y and a ≠ b
  def whereClause: Parser[List[RuleGuard]] =
    WHERE ~> rep1sep(guardExpr, AND)
  
  // Guards use simple atoms (no application that could span lines)
  def guardExpr: Parser[RuleGuard] =
    nonParenAtom ~ ("=" ~> nonParenAtom) ^^ { case l ~ r => RuleGuard("eq", l, r) } |
    nonParenAtom ~ (NEQ ~> nonParenAtom) ^^ { case l ~ r => RuleGuard("neq", l, r) }
  
  def defDecl: Parser[Def] =
    DEF ~> ident ~ opt(":" ~> ident) ~ (EQUALS ~> defBody) ^^ {
      case name ~ sort ~ body => Def(name, sort, body)
    }
  
  // Def body can be a parse expression or a term pattern
  def defBody: Parser[Pat] =
    parseExpr | termPattern
  
  // parse <grammar> """<text>""" or parse <grammar> "<text>" - returns a placeholder for now
  def parseExpr: Parser[Pat] =
    PARSE ~> ident ~ (tripleQuotedString | stringLit) ^^ {
      case grammar ~ text => Pat.PCon("parse", List(Pat.PCon(grammar, Nil), Pat.PCon(text, Nil)))
    }
  
  def tripleQuotedString: Parser[String] =
    "\"\"\"" ~> """(?s).*?(?=\"\"\")""".r <~ "\"\"\""
  
  // Token declarations (parsed but ignored for now)
  def tokenBlock: Parser[Unit] = TOKEN ~> rep(tokenLine) ^^^ ()
  def tokenLine: Parser[Unit] = nonKeywordIdent ~ opt(stringLit) ^^^ ()
  
  // Syntax declarations
  def syntaxDecl: Parser[Unit] = SYNTAX ~> ident ~ (":" ~> ident) ^^^ ()
  
  // Grammar blocks
  def grammarBlock: Parser[Unit] = 
    GRAMMAR ~> ident ~ ("{" ~> rep(grammarRule) <~ "}") ^^^ ()
  def grammarRule: Parser[Unit] = 
    rep1(grammarToken) ~ ("=>" ~> grammarAction) ^^^ ()
  def grammarToken: Parser[Unit] =
    stringLit ^^^ () | (nonKeywordIdent ~ opt("*" | "+")) ^^^ ()
  // Grammar action: just a constructor name, optionally with args
  def grammarAction: Parser[Unit] =
    ident ~ opt("(" ~> repsep(grammarArg, ",") <~ ")") ^^^ ()
  def grammarArg: Parser[Unit] =
    "_" ^^^ () | ident ^^^ ()
  
  // Convert type to string for compatibility
  def typeToString(t: LangType): String = t match
    case LangType.SortRef(s) => s
    case LangType.Product(a, b) => s"(${typeToString(a)} × ${typeToString(b)})"
    case LangType.ListOf(a) => s"${typeToString(a)}*"
    case LangType.Arrow(a, b) => s"(${typeToString(a)} → ${typeToString(b)})"
  
  // Term patterns for definitions - all idents become constructors (no metavars)
  def termPattern: Parser[Pat] = termPairPat
  
  // Top-level comma creates pairs in term patterns too
  def termPairPat: Parser[Pat] =
    termSubst ~ rep("," ~> termSubst) ^^ {
      case first ~ Nil => first
      case first ~ rest => (first :: rest).reduceRight((a, b) => Pat.PCon("pair", List(a, b)))
    }
  
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
    termListLit |
    number ^^ numeral |
    LAMBDA ~> rep1(nonKeywordIdent) ~ ("." ~> termPattern) ^^ { case params ~ body =>
      params.foldRight(body)((p, b) => Pat.PCon("lam", List(Pat.PCon(p, Nil), Pat.PVar("_"), b)))
    } |
    termConstructorPat |
    termQualifiedPat |
    nonKeywordIdent ^^ { name => Pat.PCon(name, Nil) }  // Always constructor, never metavar
  
  // List literals in term position: [a, b, c] -> cons(a, cons(b, nil))
  // Use termSubst (not termPattern) to avoid comma being treated as pair
  def termListLit: Parser[Pat] =
    "[" ~ "]" ^^^ Pat.PCon("nil", Nil) |
    "[" ~> rep1sep(termSubst, ",") <~ "]" ^^ { elems =>
      elems.foldRight(Pat.PCon("nil", Nil): Pat)((e, acc) => Pat.PCon("cons", List(e, acc)))
    }
  
  def termConstructorPat: Parser[Pat] =
    ident ~ ("(" ~> repsep(termSubst, ",") <~ ")") ^^ {
      case name ~ args => Pat.PCon(name, args)
    }
  
  // Qualified constructor: Xform.forward(args)
  def termQualifiedPat: Parser[Pat] =
    qualifiedIdent ~ ("(" ~> repsep(termSubst, ",") <~ ")") ^^ {
      case name ~ args => Pat.PCon(name, args)
    }
  
  def strategyDecl: Parser[(String, Strat)] =
    STRATEGY ~> ident ~ (ASSIGN ~> strategy) ^^ { case n ~ s => (n, s) }
  
  // =========================================================================
  // Patterns - Key difference: constructor application
  // =========================================================================
  
  def pattern: Parser[Pat] = pairPat
  
  // Top-level comma creates pairs: a, b, c -> pair(a, pair(b, c))
  def pairPat: Parser[Pat] =
    substitutionPat ~ rep("," ~> substitutionPat) ^^ {
      case first ~ Nil => first
      case first ~ rest => (first :: rest).reduceRight((a, b) => Pat.PCon("pair", List(a, b)))
    }
  
  // Pattern without top-level comma (for inside parentheses where comma means tuple)
  def patternNoComma: Parser[Pat] = consPat
  
  // Cons pattern: head::tail (right-associative) - list cons at pattern level
  def consPat: Parser[Pat] =
    substitutionPat ~ opt("::" ~> consPat) ^^ {
      case head ~ Some(tail) => Pat.PCon("cons", List(head, tail))
      case head ~ None => head
    }
  
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
    // List patterns: [], [h | t], [a, b, c]
    listPat |
    // Tuple pattern: (a, b) or parenthesized: (expr)
    // Use patternNoComma inside parens so comma creates tuple, not pair
    "(" ~> patternNoComma ~ opt("," ~> rep1sep(patternNoComma, ",")) <~ ")" ^^ {
      case first ~ None => first  // Just parenthesized
      case first ~ Some(rest) => 
        // Build right-nested pairs: (a, b, c) -> pair(a, pair(b, c))
        (first :: rest).reduceRight((a, b) => Pat.PCon("pair", List(a, b)))
    } |
    number ^^ numeral |
    LAMBDA ~> rep1(nonKeywordIdent) ~ ("." ~> patternNoComma) ^^ { case params ~ body =>
      params.foldRight(body)((p, b) => Pat.PCon("lam", List(Pat.PCon(p, Nil), Pat.PVar("_"), b)))
    } |
    constructorPat |
    qualifiedPat |
    nonKeywordIdent ^^ { name =>
      // In rule patterns, identifiers are metavariables unless they're known constructors
      if knownCons(name) then Pat.PCon(name, Nil)
      else Pat.PVar(name)
    }
  
  // List patterns: [], [h | t], [a, b, c], [a, b | t]
  def listPat: Parser[Pat] =
    // Empty list
    "[" ~ "]" ^^^ Pat.PCon("nil", Nil) |
    // Cons pattern with explicit tail: [a, b | t] or [h | t]
    "[" ~> rep1sep(patternNoComma, ",") ~ (CONS ~> patternNoComma) <~ "]" ^^ {
      case elems ~ tail => 
        elems.foldRight(tail)((e, acc) => Pat.PCon("cons", List(e, acc)))
    } |
    // List literal: [a, b, c] -> cons(a, cons(b, cons(c, nil)))
    "[" ~> rep1sep(patternNoComma, ",") <~ "]" ^^ { elems =>
      elems.foldRight(Pat.PCon("nil", Nil): Pat)((e, acc) => Pat.PCon("cons", List(e, acc)))
    }
  
  // Identifier that's not a keyword
  def nonKeywordIdent: Parser[String] = 
    ident.filter(s => !keywords.contains(s))
  
  // Constructor with explicit args: Con(a, b, c) - NO space before paren
  def constructorPat: Parser[Pat] =
    """[\p{L}_][\p{L}\p{N}_]*\(""".r >> { s =>
      val name = s.dropRight(1)  // Remove the (
      repsep(patternNoComma, ",") <~ ")" ^^ { args => Pat.PCon(name, args) }
    }
  
  // Qualified pattern: Xform.forward(args) - NO space before paren
  def qualifiedPat: Parser[Pat] =
    """[\p{L}_][\p{L}\p{N}_]*(?:\.[\p{L}_][\p{L}\p{N}_]*)*\(""".r >> { s =>
      val name = s.dropRight(1)  // Remove the (
      repsep(patternNoComma, ",") <~ ")" ^^ { args => Pat.PCon(name, args) }
    }
  
  // Known constructors - used in rule patterns to distinguish constructors from metavariables
  // These are the nullary constructors that appear in rule patterns  
  def knownCons(s: String) = Set(
    // STLC
    "zero", "succ", "unit", "lam", "app", "pair", "fst", "snd", "NatRec", "true", "false",
    // List
    "nil", "cons",
    // λProlog AST
    "Var", "Const", "Lam", "App", "True", "And", "Call", "Clause", "Program",
    // HKT
    "Star", "KArrow", "TVar", "TApp", "TArrow", "TForall", "TList", "TMaybe", 
    "TEither", "TPair", "TFix", "TyLam", "TyApp",
    // Phi self-hosting
    "LangSpec", "DSort", "DCon", "DRule", "DDef", "DStrat",
    "TName", "PVar", "PCon", "PApp", "PSubst", "PLam", "PNum",
    "SId", "SFail", "SApply", "SSeq", "SChoice", "SRepeat",
    // Tokens
    "TokId", "TokNum", "TokStr", "TokSym", "TokKw", 
    "TokLPar", "TokRPar", "TokLBra", "TokRBra", "TokLBrace", "TokRBrace",
    "TokComma", "TokColon", "TokDot", "TokEOF"
  ).contains(s)
  
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
    qualifiedIdent ^^ Strat.Apply.apply  // Support Xform.forward as strategy
  
  def parse(input: String): Either[String, LangSpec] =
    parseAll(spec, input) match
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(s"${failure.msg} at ${failure.next.pos}")
