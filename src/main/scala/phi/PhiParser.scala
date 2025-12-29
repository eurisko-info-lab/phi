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
  def ATTR = "attr"
  def INHERITED = "inherited"
  def SYNTHESIZED = "synthesized"
  
  val keywords = Set("language", "sort", "constructor", "xform", "change", "rule", "def", 
                     "repeat", "id", "where", "and", "token", "syntax", "grammar", "parse", "strategy", "theorem",
                     "attr", "inherited", "synthesized")
  
  def ARROW: Parser[String] = "→" | "->"
  def BIARROW: Parser[String] = "⇄" | "<->"
  def MAPSTO: Parser[String] = "↦" | "|->"
  def LAMBDA: Parser[String] = "λ" | "\\"
  def ASSIGN: Parser[String] = ":="
  def EQUALS: Parser[String] = "="
  def TIMES: Parser[String] = "×" | "*"
  def NEQ: Parser[String] = "≠" | "!="
  def CONS: Parser[String] = "::" | "|"  // | is safe inside [...] brackets
  def EXTENDS = "extends"
  
  // =========================================================================
  // Top Level
  // =========================================================================
  
  def spec: Parser[LangSpec] =
    LANGUAGE ~> ident ~ opt(EXTENDS ~> ident) ~ ("{" ~> rep(declaration) <~ "}") ^^ {
      case name ~ parent ~ decls =>
        val flat = decls.flatten
        LangSpec(
          name,
          flat.collect { case s: Sort => s },
          flat.collect { case c: Constructor => c },
          flat.collect { case x: XformSpec => x },
          flat.collect { case c: ChangeSpec => c },
          flat.collect { case r: Rule => r },
          flat.collect { case d: Def => d },
          flat.collect { case (n: String, s: RewriteStrategy) => (n, s) }.toMap,
          flat.collect { case t: Theorem => t },
          flat.collect { case a: AttrSpec => a },
          flat.collect { case e: AttrEquation => e },
          parent  // Store the parent language name
        )
    }
  
  def declaration: Parser[List[Any]] =
    sortDecl ^^ (List(_)) | 
    constructorBlock | 
    attrEquation ^^ (List(_)) |    // Attribute equations (before attrDecl - both start with ATTR)
    attrDecl ^^ (List(_)) |        // Attribute declarations
    xformDecl ^^ (List(_)) | 
    changeDecl ^^ (List(_)) | 
    ruleDecl ^^ (List(_)) | 
    theoremDecl ^^ (List(_)) |     // Theorem declarations
    strategyDecl ^^ (List(_)) |  // Before defDecl - both start with ident
    defDecl ^^ (List(_)) | 
    tokenBlock ^^ (_ => Nil) |     // Parse but ignore token blocks for now
    syntaxDecl ^^ (_ => Nil) |     // Parse but ignore syntax decls
    grammarBlock ^^ (_ => Nil)     // Parse but ignore grammar blocks
  
  def sortDecl: Parser[Sort] = 
    SORT ~> ident ~ opt("[" ~> rep1sep(ident, ",") <~ "]") ^^ {
      case name ~ Some(params) => Sort(name, params)
      case name ~ None => Sort(name)
    }
  
  // Attribute declaration: attr name : Type inherited|synthesized
  def attrDecl: Parser[AttrSpec] =
    ATTR ~> ident ~ (":" ~> typeExpr) ~ attrFlow ~ opt(EQUALS ~> pattern) ^^ {
      case name ~ ty ~ flow ~ default => AttrSpec(name, ty, flow, default)
    }
  
  def attrFlow: Parser[AttrFlow] =
    INHERITED ^^^ AttrFlow.Inherited | SYNTHESIZED ^^^ AttrFlow.Synthesized

  // Attribute equation: attr name(Pattern) [for child] = computation
  // Synthesized: attr type(Var(x)) = lookup(x, env)
  // Inherited:   attr env(Lam(x, body)) for body = extend(env, x, freshType)
  def attrEquation: Parser[AttrEquation] =
    ATTR ~> ident ~ ("(" ~> pattern <~ ")") ~ opt("for" ~> ident) ~ (EQUALS ~> attrComputation) ^^ {
      case name ~ pat ~ forChild ~ comp => AttrEquation(name, pat, forChild, comp)
    }

  // Computation expression for attribute equations
  // Allows: lookup(x, env), extend(env, x, t), type(f), etc.
  def attrComputation: Parser[MetaPattern] =
    attrComputationApp

  def attrComputationApp: Parser[MetaPattern] =
    attrComputationAtom ~ opt("(" ~> rep1sep(attrComputation, ",") <~ ")") ^^ {
      case func ~ Some(args) => func match
        case MetaPattern.PVar(name) => MetaPattern.PCon(name, args)
        case MetaPattern.PCon(name, Nil) => MetaPattern.PCon(name, args)
        case _ => args.foldLeft(func)((f, a) => MetaPattern.PCon("app", List(f, a)))
      case atom ~ None => atom
    }

  def attrComputationAtom: Parser[MetaPattern] =
    "(" ~> attrComputation <~ ")" |
    number ^^ numeral |
    nonKeywordIdent ^^ { name => 
      // Uppercase identifiers > 1 char are constructors (like TInt, True)
      // Single uppercase letters and lowercase are variables (x, A, env)
      if name.length > 1 && name.headOption.exists(_.isUpper) && !isIndexedVar(name) then
        MetaPattern.PCon(name, Nil)
      else
        MetaPattern.PVar(name)
    }

  // Constructor declaration: either single line or block
  // Single: constructor Foo : A -> B
  // Block: constructor
  //          Foo : A -> B
  //          Bar : C -> D
  // Polymorphic: constructor None[A] : Option[A]
  def constructorBlock: Parser[List[Constructor]] =
    CONSTRUCTOR ~> (
      rep1(constructorLine) |  // Block: constructor followed by indented lines
      constructorLine ^^ (List(_))  // Single line on same line
    )
  
  def constructorLine: Parser[Constructor] =
    ident ~ opt("[" ~> rep1sep(ident, ",") <~ "]") ~ (":" ~> constructorType) ^^ {
      case name ~ typeParams ~ ((params, ret)) => 
        // For now, we store type params in the constructor name for simplicity
        // A more complete impl would add a typeParams field to Constructor
        val fullName = typeParams match
          case Some(ps) => s"$name[${ps.mkString(",")}]"
          case None => name
        Constructor(fullName, params, ret)
    }
  
  def constructorType: Parser[(List[(Option[String], LangType)], String)] =
    rep(typeArg <~ ARROW) ~ returnType ^^ { case params ~ ret => (params, ret) }
  
  // Return type can be a polymorphic type like Option[A] or just Term
  def returnType: Parser[String] =
    ident ~ opt("[" ~> rep1sep(typeExpr, ",") <~ "]") ^^ {
      case name ~ Some(args) => s"$name[${args.map(t => PhiParser.typeToString(t)).mkString(",")}]"
      case name ~ None => name
    }
  
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
    ident ~ opt("[" ~> rep1sep(typeExpr, ",") <~ "]") ^^ {
      case name ~ Some(args) => LangType.TypeApp(name, args)
      case name ~ None => LangType.SortRef(name)
    }
  
  def xformDecl: Parser[XformSpec] =
    XFORM ~> ident ~ opt("[" ~> rep1sep(ident, ",") <~ "]") ~ (":" ~> typeExpr) ~ (BIARROW ~> typeExpr) ^^ {
      case name ~ typeParams ~ src ~ tgt => 
        val fullName = typeParams match
          case Some(ps) => s"$name[${ps.mkString(",")}]"
          case None => name
        XformSpec(fullName, typeToString(src), typeToString(tgt))
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
  // Also supports type params: rule Map.forward[A,B] { ... }
  def ruleDecl: Parser[Rule] =
    RULE ~> qualifiedIdent ~ opt("[" ~> rep1sep(ident, ",") <~ "]") ~ ("{" ~> rep1(ruleCase) <~ "}") ^^ {
      case name ~ typeParams ~ cases => 
        val fullName = typeParams match
          case Some(ps) => s"$name[${ps.mkString(",")}]"
          case None => name
        Rule(fullName, RuleDir.Both, cases)
    }
  
  def ruleCase: Parser[RuleCase] =
    pattern ~ (MAPSTO ~> ruleRhs) ~ opt(whereClause) ^^ {
      case lhs ~ rhs ~ guards => RuleCase(lhs, rhs, guards.getOrElse(Nil))
    }
  
  // RHS of a rule - single atom, or constructor with parens args, or application of atoms
  // Also handles ++ (list concat) and other binary ops
  def ruleRhs: Parser[MetaPattern] =
    rhsConcat
  
  def rhsConcat: Parser[MetaPattern] =
    rhsApp ~ opt("++" ~> rhsApp) ^^ {
      case l ~ Some(r) => MetaPattern.PCon("concat", List(l, r))
      case l ~ None => l
    }
  
  def rhsApp: Parser[MetaPattern] =
    constructorPat |  // Con(a, b, c) - explicit args in parens
    qualifiedPat |    // Xform.forward(args)
    rep1(rhsAtom) ~ opt("[" ~> ident ~ (ASSIGN ~> pattern) <~ "]") ^^ {
      case atoms ~ None => 
        atoms.reduceLeft((f, a) => MetaPattern.PCon("app", List(f, a)))
      case atoms ~ Some(v ~ repl) =>
        val base = atoms.reduceLeft((f, a) => MetaPattern.PCon("app", List(f, a)))
        MetaPattern.PSubst(base, v, repl)
    }
  
  // Atom allowed in RHS - includes parenthesized expressions that aren't tuple LHS patterns
  // A tuple LHS would be (a, b, c), Foo... - with comma at end
  // A function arg would be (Subst.forward(...)) - application inside parens
  // NOTE: We don't include constructorPat here to avoid accidentally grabbing 
  // the next rule's LHS pattern like Baz(y) when parsing RHS of previous rule.
  // If you need Con(args) on RHS, rhsApp handles it at the top level.
  // Identifiers must not be immediately followed by '(' (use possessive quantifier + negative lookahead)
  def rhsAtom: Parser[MetaPattern] =
    listPat |
    number ^^ numeral |
    qualifiedPat |
    // Parenthesized application (not tuple-like) - starts with uppercase or qualified
    "(" ~> applicationPat <~ ")" |
    // Identifier NOT immediately followed by '(' - regex handles this at char level
    """[\p{L}_][\p{L}\p{N}_]*+(?!\()""".r.filter(s => !keywords.contains(s)) ^^ { name =>
      // Uppercase identifiers > 1 char are constructors (e.g., True, Foo)
      // Single uppercase letters are variables (e.g., X, A, B)
      // Indexed variables like X_1, A_n are also variables
      if knownCons(name) || (name.length > 1 && name.headOption.exists(_.isUpper) && !isIndexedVar(name)) then MetaPattern.PCon(name, Nil)
      else MetaPattern.PVar(name)
    }
  
  // Atom that's not a parenthesized expression (for guards)
  def nonParenAtom: Parser[MetaPattern] =
    listPat |
    number ^^ numeral |
    constructorPat |
    qualifiedPat |
    nonKeywordIdent ^^ { name =>
      if knownCons(name) || (name.length > 1 && name.headOption.exists(_.isUpper) && !isIndexedVar(name)) then MetaPattern.PCon(name, Nil)
      else MetaPattern.PVar(name)
    }
  
  // where x = y and a ≠ b
  def whereClause: Parser[List[RuleGuard]] =
    WHERE ~> rep1sep(guardExpr, AND)
  
  // Guards use simple atoms (no application that could span lines)
  def guardExpr: Parser[RuleGuard] =
    nonParenAtom ~ ("=" ~> nonParenAtom) ^^ { case l ~ r => RuleGuard("eq", l, r) } |
    nonParenAtom ~ (NEQ ~> nonParenAtom) ^^ { case l ~ r => RuleGuard("neq", l, r) }
  
  // def foo : Type = body
  // def foo[A,B] : Type = body (polymorphic)
  def defDecl: Parser[Def] =
    DEF ~> ident ~ opt("[" ~> rep1sep(ident, ",") <~ "]") ~ opt(":" ~> defType) ~ (EQUALS ~> defBody) ^^ {
      case name ~ typeParams ~ sort ~ body => 
        val fullName = typeParams match
          case Some(ps) => s"$name[${ps.mkString(",")}]"
          case None => name
        Def(fullName, sort.flatten, body)
    }
  
  // Type annotation for defs - can be just a sort name or a full type expression
  def defType: Parser[Option[String]] =
    typeExpr ^^ { ty => Some(typeToString(ty)) }
  
  // Def body can be a parse expression or a term pattern
  def defBody: Parser[MetaPattern] =
    parseExpr | termPattern
  
  // parse <grammar> """<text>""" or parse <grammar> "<text>" - returns a placeholder for now
  def parseExpr: Parser[MetaPattern] =
    PARSE ~> ident ~ (tripleQuotedString | stringLit) ^^ {
      case grammar ~ text => MetaPattern.PCon("parse", List(MetaPattern.PCon(grammar, Nil), MetaPattern.PCon(text, Nil)))
    }
  
  def tripleQuotedString: Parser[String] =
    "\"\"\"" ~> """(?s).*?(?=\"\"\")""".r <~ "\"\"\""
  
  // Token declarations (parsed but ignored for now)
  def tokenBlock: Parser[Unit] = TOKEN ~> rep(tokenLine) ^^^ ()
  def tokenLine: Parser[Unit] = nonKeywordIdent ~ opt(stringLit) ^^^ ()
  
  // Syntax declarations - can be simple `syntax X : Y` or complex `syntax "kw" ~ pattern...`
  def syntaxDecl: Parser[Unit] = SYNTAX ~> syntaxPattern ^^^ ()
  def syntaxPattern: Parser[Unit] = 
    syntaxSeq ~ rep("|" ~> syntaxSeq) ^^^ ()
  def syntaxSeq: Parser[Unit] =
    syntaxAtom ~ rep("~" ~> syntaxAtom) ^^^ ()
  def syntaxAtom: Parser[Unit] =
    stringLit ^^^ () |
    ("(" ~> syntaxPattern <~ ")") |
    ident ~ opt(":" ~> ident) ^^^ ()
  
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
    case LangType.TypeApp(base, args) => s"$base[${args.map(typeToString).mkString(",")}]"
    case LangType.TypeVar(v) => v
    case LangType.Product(a, b) => s"(${typeToString(a)} × ${typeToString(b)})"
    case LangType.ListOf(a) => s"${typeToString(a)}*"
    case LangType.Arrow(a, b) => s"(${typeToString(a)} → ${typeToString(b)})"
  
  // Term patterns for definitions - all idents become constructors (no metavars)
  def termPattern: Parser[MetaPattern] = termPairPat
  
  // Top-level comma creates pairs in term patterns too
  def termPairPat: Parser[MetaPattern] =
    termSubst ~ rep("," ~> termSubst) ^^ {
      case first ~ Nil => first
      case first ~ rest => (first :: rest).reduceRight((a, b) => MetaPattern.PCon("pair", List(a, b)))
    }
  
  def termSubst: Parser[MetaPattern] =
    termApp ~ opt("[" ~> ident ~ (ASSIGN ~> termPattern) <~ "]") ^^ {
      case body ~ Some(v ~ repl) => MetaPattern.PSubst(body, v, repl)
      case body ~ None => body
    }
  
  def termApp: Parser[MetaPattern] =
    rep1(termAtom) ^^ { atoms =>
      atoms.reduceLeft((f, a) => MetaPattern.PCon("app", List(f, a)))
    }
  
  def termAtom: Parser[MetaPattern] =
    "(" ~> termPattern <~ ")" |
    termListLit |
    number ^^ numeral |
    stringLit ^^ { s => MetaPattern.PCon(s, Nil) } |  // String literals become constructors
    LAMBDA ~> rep1(nonKeywordIdent) ~ ("." ~> termPattern) ^^ { case params ~ body =>
      params.foldRight(body)((p, b) => MetaPattern.PCon("lam", List(MetaPattern.PCon(p, Nil), MetaPattern.PVar("_"), b)))
    } |
    termConstructorPat |
    termQualifiedPat |
    nonKeywordIdent ^^ { name => MetaPattern.PCon(name, Nil) }  // Always constructor, never metavar
  
  // List literals in term position: [a, b, c] -> cons(a, cons(b, nil))
  // Use termSubst (not termPattern) to avoid comma being treated as pair
  def termListLit: Parser[MetaPattern] =
    "[" ~ "]" ^^^ MetaPattern.PCon("nil", Nil) |
    "[" ~> rep1sep(termSubst, ",") <~ "]" ^^ { elems =>
      elems.foldRight(MetaPattern.PCon("nil", Nil): MetaPattern)((e, acc) => MetaPattern.PCon("cons", List(e, acc)))
    }
  
  def termConstructorPat: Parser[MetaPattern] =
    ident ~ ("(" ~> repsep(termSubst, ",") <~ ")") ^^ {
      case name ~ args => MetaPattern.PCon(name, args)
    }
  
  // Qualified constructor: Xform.forward(args)
  def termQualifiedPat: Parser[MetaPattern] =
    qualifiedIdent ~ ("(" ~> repsep(termSubst, ",") <~ ")") ^^ {
      case name ~ args => MetaPattern.PCon(name, args)
    }
  
  def strategyDecl: Parser[(String, RewriteStrategy)] =
    STRATEGY ~> ident ~ (ASSIGN ~> strategy) ^^ { case n ~ s => (n, s) }
  
  // =========================================================================
  // Patterns - Key difference: constructor application
  // =========================================================================
  
  def pattern: Parser[MetaPattern] = pairPat
  
  // Top-level comma creates pairs: a, b, c -> pair(a, pair(b, c))
  def pairPat: Parser[MetaPattern] =
    substitutionPat ~ rep("," ~> substitutionPat) ^^ {
      case first ~ Nil => first
      case first ~ rest => (first :: rest).reduceRight((a, b) => MetaPattern.PCon("pair", List(a, b)))
    }
  
  // Pattern without top-level comma (for inside parentheses where comma means tuple)
  def patternNoComma: Parser[MetaPattern] = consPat
  
  // Cons pattern: head::tail (right-associative) - list cons at pattern level
  def consPat: Parser[MetaPattern] =
    substitutionPat ~ opt("::" ~> consPat) ^^ {
      case head ~ Some(tail) => MetaPattern.PCon("cons", List(head, tail))
      case head ~ None => head
    }
  
  def substitutionPat: Parser[MetaPattern] =
    applicationPat ~ opt("[" ~> ident ~ (ASSIGN ~> pattern) <~ "]") ^^ {
      case body ~ Some(v ~ repl) => MetaPattern.PSubst(body, v, repl)
      case body ~ None => body
    }
  
  // Application is left-associative: f a b = (f a) b
  def applicationPat: Parser[MetaPattern] =
    rep1(atomPat) ^^ { atoms =>
      atoms.reduceLeft((f, a) => MetaPattern.PCon("app", List(f, a)))
    }
  
  def atomPat: Parser[MetaPattern] =
    // List patterns: [], [h | t], [a, b, c]
    listPat |
    // Tuple pattern: (a, b) or parenthesized: (expr)
    // Use patternNoComma inside parens so comma creates tuple, not pair
    "(" ~> patternNoComma ~ opt("," ~> rep1sep(patternNoComma, ",")) <~ ")" ^^ {
      case first ~ None => first  // Just parenthesized
      case first ~ Some(rest) => 
        // Build right-nested pairs: (a, b, c) -> pair(a, pair(b, c))
        (first :: rest).reduceRight((a, b) => MetaPattern.PCon("pair", List(a, b)))
    } |
    number ^^ numeral |
    stringLit ^^ { s => MetaPattern.PCon(s, Nil) } |  // String literals become nullary constructors
    LAMBDA ~> rep1(nonKeywordIdent) ~ ("." ~> patternNoComma) ^^ { case params ~ body =>
      params.foldRight(body)((p, b) => MetaPattern.PCon("lam", List(MetaPattern.PCon(p, Nil), MetaPattern.PVar("_"), b)))
    } |
    constructorPat |
    qualifiedPat |
    nonKeywordIdent ^^ { name =>
      // In rule patterns, identifiers are metavariables unless they're known constructors
      // or uppercase with length > 1 (e.g., True, Foo are constructors; X, A, X_1 are variables)
      if knownCons(name) || (name.length > 1 && name.headOption.exists(_.isUpper) && !isIndexedVar(name)) then MetaPattern.PCon(name, Nil)
      else MetaPattern.PVar(name)
    }
  
  // List patterns: [], [h | t], [a, b, c], [a, b | t]
  def listPat: Parser[MetaPattern] =
    // Empty list
    "[" ~ "]" ^^^ MetaPattern.PCon("nil", Nil) |
    // Cons pattern with explicit tail: [a, b | t] or [h | t]
    "[" ~> rep1sep(patternNoComma, ",") ~ (CONS ~> patternNoComma) <~ "]" ^^ {
      case elems ~ tail => 
        elems.foldRight(tail)((e, acc) => MetaPattern.PCon("cons", List(e, acc)))
    } |
    // List literal: [a, b, c] -> cons(a, cons(b, cons(c, nil)))
    "[" ~> rep1sep(patternNoComma, ",") <~ "]" ^^ { elems =>
      elems.foldRight(MetaPattern.PCon("nil", Nil): MetaPattern)((e, acc) => MetaPattern.PCon("cons", List(e, acc)))
    }
  
  // Identifier that's not a keyword
  def nonKeywordIdent: Parser[String] = 
    ident.filter(s => !keywords.contains(s))
  
  // Constructor with explicit args: Con(a, b, c) - NO space before paren
  def constructorPat: Parser[MetaPattern] =
    """[\p{L}_][\p{L}\p{N}_]*\(""".r >> { s =>
      val name = s.dropRight(1)  // Remove the (
      repsep(patternNoComma, ",") <~ ")" ^^ { args => MetaPattern.PCon(name, args) }
    }
  
  // Qualified pattern: Xform.forward(args) - requires at least one dot, NO space before paren
  def qualifiedPat: Parser[MetaPattern] =
    """[\p{L}_][\p{L}\p{N}_]*(?:\.[\p{L}_][\p{L}\p{N}_]*)+\(""".r >> { s =>
      val name = s.dropRight(1)  // Remove the (
      repsep(patternNoComma, ",") <~ ")" ^^ { args => MetaPattern.PCon(name, args) }
    }
  
  // Check if name is an indexed variable like X_1, A_n, Foo_123
  // Pattern: single uppercase letter followed by underscore and alphanumeric
  def isIndexedVar(name: String): Boolean =
    name.length >= 3 && 
    name.charAt(0).isUpper && 
    name.charAt(1) == '_' &&
    name.drop(2).forall(c => c.isLetterOrDigit)
  
  // Known constructors - used in rule patterns to distinguish constructors from metavariables
  // These are the nullary constructors that appear in rule patterns  
  def knownCons(s: String) = Set(
    // STLC
    "zero", "succ", "unit", "lam", "app", "pair", "fst", "snd", "NatRec", "true", "false",
    // Prolog
    "bob", "alice",
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
  
  def numeral(n: Int): MetaPattern =
    if n == 0 then MetaPattern.PCon("zero", Nil)
    else MetaPattern.PCon("succ", List(numeral(n - 1)))
  
  // =========================================================================
  // Strategies
  // =========================================================================
  
  def strategy: Parser[RewriteStrategy] =
    strategySeq ~ rep("|" ~> strategySeq) ^^ {
      case first ~ rest => rest.foldLeft(first)(RewriteStrategy.Choice.apply)
    }
  
  def strategySeq: Parser[RewriteStrategy] =
    strategyAtom ~ rep(";" ~> strategyAtom) ^^ {
      case first ~ rest => rest.foldLeft(first)(RewriteStrategy.Seq.apply)
    }
  
  def strategyAtom: Parser[RewriteStrategy] =
    "(" ~> strategy <~ ")" |
    "repeat" ~> strategyAtom ^^ RewriteStrategy.Repeat.apply |
    "all" ~> strategyAtom ^^ RewriteStrategy.All.apply |  // Non-deterministic: all results
    "id" ^^^ RewriteStrategy.Id |
    qualifiedIdent ^^ RewriteStrategy.Apply.apply  // Support Xform.forward as strategy
  
  def parse(input: String): Either[String, LangSpec] =
    parseAll(spec, input) match
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(s"${failure.msg} at ${failure.next.pos}")
