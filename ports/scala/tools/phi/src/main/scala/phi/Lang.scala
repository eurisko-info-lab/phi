package phi

/**
 * ╔═══════════════════════════════════════════════════════════════════════════╗
 * ║                     Φ-LANG: Language Specification                        ║
 * ╠═══════════════════════════════════════════════════════════════════════════╣
 * ║  Parse and represent .phi language specification files                    ║
 * ╚═══════════════════════════════════════════════════════════════════════════╝
 *
 * WHAT IS A .phi FILE?
 * ====================
 * 
 * A .phi file defines a complete language specification:
 *   - Sorts (types): what kinds of things exist
 *   - Constructors: how to build values of each sort
 *   - Grammars: concrete syntax for parsing/printing
 *   - Transforms: AST-to-AST transformations
 *   - Rules: operational semantics / rewrite rules
 *
 * EXAMPLE .phi FILE
 * =================
 * 
 *   // Define the sorts (types) of our language
 *   sort Expr
 *   sort Stmt
 *   
 *   // Constructors for each sort
 *   Expr = Lit(value: Int) 
 *        | Add(left: Expr, right: Expr)
 *        | Var(name: String)
 *   
 *   // Concrete syntax (grammar)
 *   grammar Expr {
 *     Lit  <- /[0-9]+/
 *     Add  <- Expr "+" Expr
 *     Var  <- /[a-z]+/
 *   }
 *   
 *   // Transformation rules
 *   xform optimize : Expr -> Expr {
 *     Add(Lit(0), e) => e
 *     Add(e, Lit(0)) => e
 *   }
 *
 * ARCHITECTURE
 * ============
 * 
 *   LangSpec
 *   ├── sorts: List[SortDecl]        // Type declarations
 *   ├── constructors: List[ConDecl]  // Data constructors
 *   ├── grammars: List[GrammarDecl]  // Concrete syntax
 *   ├── xforms: List[XformDecl]      // Transformations
 *   └── rules: List[RuleDecl]        // Semantics
 */
object Lang:
  import Core.*
  import Core.Val.*
  import Meta.*
  import scala.util.parsing.combinator.*

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 1: AST for Language Specifications
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * LangSpec: Complete specification of a language.
   * 
   * This is the root of a parsed .phi file.
   */
  case class LangSpec(
    name: String,
    sorts: List[SortDecl],
    constructors: List[ConDecl],
    grammars: List[GrammarDecl],
    xforms: List[XformDecl],
    rules: List[RuleDecl]
  ):
    /** Get all constructors for a sort */
    def consFor(sort: String): List[ConDecl] =
      constructors.filter(_.sort == sort)
    
    /** Get grammar rules for a sort */
    def grammarFor(sort: String): Option[GrammarDecl] =
      grammars.find(_.sort == sort)
    
    /** Lookup a transform by name */
    def xformNamed(name: String): Option[XformDecl] =
      xforms.find(_.name == name)
    
    /** Pretty-print the spec */
    def show: String =
      val sb = new StringBuilder
      sb ++= s"// Language: $name\n\n"
      
      if sorts.nonEmpty then
        sb ++= "// Sorts\n"
        sorts.foreach(s => sb ++= s"sort ${s.name}${s.params.map(p => s"[$p]").mkString}\n")
        sb ++= "\n"
      
      if constructors.nonEmpty then
        sb ++= "// Constructors\n"
        constructors.groupBy(_.sort).foreach { (sort, cons) =>
          sb ++= s"$sort = ${cons.map(_.show).mkString("\n     | ")}\n"
        }
        sb ++= "\n"
      
      if grammars.nonEmpty then
        sb ++= "// Grammars\n"
        grammars.foreach(g => sb ++= g.show + "\n")
      
      sb.result()

  /**
   * SortDecl: Declaration of a sort (type).
   * 
   * Examples:
   *   sort Expr           -- simple sort
   *   sort List[A]        -- parameterized sort
   *   sort Map[K, V]      -- multi-parameter sort
   */
  case class SortDecl(
    name: String,
    params: List[String] = Nil
  ):
    def show: String = 
      if params.isEmpty then s"sort $name"
      else s"sort $name[${params.mkString(", ")}]"

  /**
   * ConDecl: Constructor declaration.
   * 
   * Examples:
   *   Lit(value: Int)              -- constructor with typed field
   *   Add(left: Expr, right: Expr) -- binary constructor
   *   Nil                          -- nullary constructor
   */
  case class ConDecl(
    sort: String,
    name: String,
    fields: List[Field]
  ):
    def arity: Int = fields.length
    def show: String = 
      if fields.isEmpty then name
      else s"$name(${fields.map(_.show).mkString(", ")})"

  /**
   * Field: A field in a constructor.
   */
  case class Field(name: String, typ: TypeRef):
    def show: String = s"$name: ${typ.show}"

  /**
   * TypeRef: Reference to a type (sort).
   */
  enum TypeRef:
    case Simple(name: String)
    case Applied(name: String, args: List[TypeRef])
    case ListOf(elem: TypeRef)
    
    def show: String = this match
      case Simple(n) => n
      case Applied(n, args) => s"$n[${args.map(_.show).mkString(", ")}]"
      case ListOf(elem) => s"List[${elem.show}]"

  /**
   * GrammarDecl: Concrete syntax for a sort.
   */
  case class GrammarDecl(
    sort: String,
    productions: List[Production]
  ):
    def show: String =
      s"grammar $sort {\n${productions.map(p => s"  ${p.show}").mkString("\n")}\n}"

  /**
   * Production: Single grammar production.
   */
  case class Production(
    constructor: String,
    pattern: GrammarPat
  ):
    def show: String = s"$constructor <- ${pattern.show}"

  /**
   * GrammarPat: Pattern in a grammar production.
   */
  enum GrammarPat:
    case Literal(text: String)       // "+"
    case Regex(pattern: String)      // /[0-9]+/
    case NonTerminal(sort: String)   // Expr
    case Seq(parts: List[GrammarPat]) // A B C
    case Alt(options: List[GrammarPat]) // A | B
    case Opt(inner: GrammarPat)      // A?
    case Rep(inner: GrammarPat)      // A*
    case Rep1(inner: GrammarPat)     // A+
    case Group(inner: GrammarPat)    // (A B)
    
    def show: String = this match
      case Literal(t)     => s"\"$t\""
      case Regex(p)       => s"/$p/"
      case NonTerminal(s) => s
      case Seq(ps)        => ps.map(_.show).mkString(" ")
      case Alt(os)        => os.map(_.show).mkString(" | ")
      case Opt(i)         => s"${i.show}?"
      case Rep(i)         => s"${i.show}*"
      case Rep1(i)        => s"${i.show}+"
      case Group(i)       => s"(${i.show})"

  /**
   * XformDecl: AST transformation.
   */
  case class XformDecl(
    name: String,
    from: String,
    to: String,
    cases: List[XformCase]
  ):
    def show: String =
      s"xform $name : $from -> $to {\n${cases.map(c => s"  ${c.show}").mkString("\n")}\n}"

  /**
   * XformCase: Single transformation case.
   */
  case class XformCase(pattern: Pat, body: Expr):
    def show: String = s"${pattern.show} => ${body.show}"

  /**
   * RuleDecl: Operational semantics rule.
   */
  case class RuleDecl(
    name: String,
    premises: List[Judgment],
    conclusion: Judgment
  ):
    def show: String =
      if premises.isEmpty then conclusion.show
      else s"${premises.map(_.show).mkString(", ")} / ${conclusion.show}"

  /**
   * Judgment: A semantic judgment.
   */
  case class Judgment(relation: String, terms: List[Pat]):
    def show: String = s"${terms.map(_.show).mkString(s" $relation ")}"

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 2: Parser for .phi Files
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * PhiParser: Parser for .phi specification files.
   * 
   * Uses Scala's parser combinators for a clean implementation.
   */
  object PhiParser extends RegexParsers:
    // Skip whitespace and comments
    override val whiteSpace = """(\s|//[^\n]*)+""".r
    
    // ─────────────────────────────────────────────────────────────────────────
    // Lexical Elements
    // ─────────────────────────────────────────────────────────────────────────
    
    def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
    def intLit: Parser[Int] = """-?[0-9]+""".r ^^ (_.toInt)
    def strLit: Parser[String] = "\"" ~> """[^"]*""".r <~ "\""
    def regex: Parser[String] = "/" ~> """[^/]+""".r <~ "/"
    
    // ─────────────────────────────────────────────────────────────────────────
    // Type References
    // ─────────────────────────────────────────────────────────────────────────
    
    def typeRef: Parser[TypeRef] =
      ("List" ~> "[" ~> typeRef <~ "]") ^^ (TypeRef.ListOf(_)) |
      (ident ~ opt("[" ~> rep1sep(typeRef, ",") <~ "]")) ^^ {
        case name ~ None => TypeRef.Simple(name)
        case name ~ Some(args) => TypeRef.Applied(name, args)
      }
    
    // ─────────────────────────────────────────────────────────────────────────
    // Sort Declarations
    // ─────────────────────────────────────────────────────────────────────────
    
    def sortDecl: Parser[SortDecl] =
      ("sort" ~> ident ~ opt("[" ~> rep1sep(ident, ",") <~ "]")) ^^ {
        case name ~ params => SortDecl(name, params.getOrElse(Nil))
      }
    
    // ─────────────────────────────────────────────────────────────────────────
    // Constructor Declarations
    // ─────────────────────────────────────────────────────────────────────────
    
    def field: Parser[Field] =
      (ident ~ (":" ~> typeRef)) ^^ { case n ~ t => Field(n, t) }
    
    def fields: Parser[List[Field]] =
      "(" ~> repsep(field, ",") <~ ")" | success(Nil)
    
    def singleCon: Parser[ConDecl] =
      (ident ~ fields) ^^ { case name ~ fs => ConDecl("", name, fs) }
    
    def conDecls: Parser[List[ConDecl]] =
      (ident <~ "=") ~ rep1sep(singleCon, "|") ^^ {
        case sort ~ cons => cons.map(c => ConDecl(sort, c.name, c.fields))
      }
    
    // ─────────────────────────────────────────────────────────────────────────
    // Grammar Productions
    // ─────────────────────────────────────────────────────────────────────────
    
    def grammarPat: Parser[GrammarPat] = grammarAlt
    
    def grammarAlt: Parser[GrammarPat] =
      rep1sep(grammarSeq, "|") ^^ {
        case List(single) => single
        case alts => GrammarPat.Alt(alts)
      }
    
    def grammarSeq: Parser[GrammarPat] =
      // Only consume up to but not including an identifier followed by <-
      rep1(not(ident ~ "<-") ~> grammarPostfix) ^^ {
        case List(single) => single
        case parts => GrammarPat.Seq(parts)
      }
    
    def grammarPostfix: Parser[GrammarPat] =
      grammarAtom ~ opt("?" | "*" | "+") ^^ {
        case base ~ None => base
        case base ~ Some("?") => GrammarPat.Opt(base)
        case base ~ Some("*") => GrammarPat.Rep(base)
        case base ~ Some("+") => GrammarPat.Rep1(base)
        case base ~ Some(_) => base // fallback for exhaustiveness
      }
    
    def grammarAtom: Parser[GrammarPat] =
      strLit ^^ GrammarPat.Literal.apply |
      regex ^^ GrammarPat.Regex.apply |
      ident ^^ GrammarPat.NonTerminal.apply |
      ("(" ~> grammarPat <~ ")") ^^ GrammarPat.Group.apply
    
    def production: Parser[Production] =
      (ident <~ "<-") ~ grammarPat ^^ { case con ~ pat => Production(con, pat) }
    
    def grammarDecl: Parser[GrammarDecl] =
      ("grammar" ~> ident) ~ ("{" ~> rep(production) <~ "}") ^^ {
        case sort ~ prods => GrammarDecl(sort, prods)
      }
    
    // ─────────────────────────────────────────────────────────────────────────
    // Patterns
    // ─────────────────────────────────────────────────────────────────────────
    
    def pattern: Parser[Pat] = patternOr
    
    def patternOr: Parser[Pat] =
      patternAs ~ rep("|" ~> patternAs) ^^ {
        case first ~ Nil => first
        case first ~ rest => rest.foldLeft(first)(POr(_, _))
      }
    
    def patternAs: Parser[Pat] =
      (ident <~ "@") ~ patternAtom ^^ { case n ~ p => PAs(n, p) } |
      patternAtom
    
    def patternAtom: Parser[Pat] =
      "_" ^^^ PWild |
      intLit ^^ (n => PLit(VInt(n))) |
      strLit ^^ (s => PLit(VStr(s))) |
      (ident ~ opt("(" ~> repsep(pattern, ",") <~ ")")) ^^ {
        case name ~ None => PVar(name)
        case name ~ Some(args) => PCon(name, args)
      } |
      ("[" ~> repsep(pattern, ",") <~ "]") ^^ PList.apply |
      ("(" ~> pattern <~ ")")
    
    // ─────────────────────────────────────────────────────────────────────────
    // Expressions
    // ─────────────────────────────────────────────────────────────────────────
    
    def expr: Parser[Expr] = exprLet | exprMatch | exprIf | exprApp
    
    def exprLet: Parser[Expr] =
      ("let" ~> ident <~ "=") ~ expr ~ ("in" ~> expr) ^^ {
        case name ~ value ~ body => ELet(name, value, body)
      }
    
    def exprMatch: Parser[Expr] =
      ("match" ~> expr <~ "{") ~ rep(matchCase) <~ "}" ^^ {
        case scrut ~ cases => EMatch(scrut, cases)
      }
    
    def matchCase: Parser[(Pat, Expr)] =
      (pattern <~ "=>") ~ expr ^^ { case p ~ e => (p, e) }
    
    def exprIf: Parser[Expr] =
      ("if" ~> expr) ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ {
        case c ~ t ~ e => EIf(c, t, e)
      }
    
    def exprApp: Parser[Expr] =
      rep1(exprAtom) ^^ { 
        case Nil => throw new RuntimeException("rep1 should never return empty list")
        case List(single) => single
        case head :: tail => tail.foldLeft(head)(EApp(_, _))
      }
    
    def exprAtom: Parser[Expr] =
      intLit ^^ (n => ELit(VInt(n))) |
      strLit ^^ (s => ELit(VStr(s))) |
      ("\\" ~> ident <~ "->") ~ expr ^^ { case p ~ b => ELam(p, b) } |
      (ident ~ opt("(" ~> repsep(expr, ",") <~ ")")) ^^ {
        case name ~ None => EVar(name)
        case name ~ Some(args) => ECon(name, args)
      } |
      ("[" ~> repsep(expr, ",") <~ "]") ^^ EList.apply |
      ("(" ~> expr <~ ")")
    
    // ─────────────────────────────────────────────────────────────────────────
    // Transform Declarations
    // ─────────────────────────────────────────────────────────────────────────
    
    def xformCase: Parser[XformCase] =
      (pattern <~ "=>") ~ expr ^^ { case p ~ e => XformCase(p, e) }
    
    def xformDecl: Parser[XformDecl] =
      ("xform" ~> ident) ~ (":" ~> ident) ~ ("->" ~> ident) ~ 
      ("{" ~> rep(xformCase) <~ "}") ^^ {
        case name ~ from ~ to ~ cases => XformDecl(name, from, to, cases)
      }
    
    // ─────────────────────────────────────────────────────────────────────────
    // Rule Declarations
    // ─────────────────────────────────────────────────────────────────────────
    
    def judgment: Parser[Judgment] =
      rep1sep(pattern, ident) ^^ {
        case List(single) => Judgment("", List(single))
        case terms => 
          // Extract relation from middle
          Judgment("⊢", terms)
      }
    
    def ruleDecl: Parser[RuleDecl] =
      ("rule" ~> ident) ~ opt(":" ~> rep1sep(judgment, ",") <~ "/") ~ judgment ^^ {
        case name ~ premises ~ concl => RuleDecl(name, premises.getOrElse(Nil), concl)
      }
    
    // ─────────────────────────────────────────────────────────────────────────
    // Top-Level
    // ─────────────────────────────────────────────────────────────────────────
    
    def declaration: Parser[Either[SortDecl, Either[List[ConDecl], 
                            Either[GrammarDecl, Either[XformDecl, RuleDecl]]]]] =
      sortDecl ^^ (Left(_)) |
      conDecls ^^ (cs => Right(Left(cs))) |
      grammarDecl ^^ (g => Right(Right(Left(g)))) |
      xformDecl ^^ (x => Right(Right(Right(Left(x))))) |
      ruleDecl ^^ (r => Right(Right(Right(Right(r)))))
    
    def langSpec(name: String): Parser[LangSpec] =
      rep(declaration) ^^ { decls =>
        val sorts = decls.collect { case Left(s) => s }
        val cons = decls.collect { case Right(Left(cs)) => cs }.flatten
        val grammars = decls.collect { case Right(Right(Left(g))) => g }
        val xforms = decls.collect { case Right(Right(Right(Left(x)))) => x }
        val rules = decls.collect { case Right(Right(Right(Right(r)))) => r }
        LangSpec(name, sorts, cons, grammars, xforms, rules)
      }
    
    /** Parse a .phi file */
    def parseFile(name: String, input: String): Either[String, LangSpec] =
      parseAll(langSpec(name), input) match
        case Success(spec, _) => Right(spec)
        case Failure(msg, next) => 
          Left(s"Parse error at line ${next.pos.line}, column ${next.pos.column}: $msg")
        case Error(msg, next) =>
          Left(s"Fatal parse error at line ${next.pos.line}, column ${next.pos.column}: $msg")

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 3: Language Operations
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Apply a transform from a LangSpec to a value.
   */
  def applyXform(spec: LangSpec, xformName: String, v: Val): Option[Val] =
    spec.xformNamed(xformName).flatMap { xform =>
      xform.cases.view.flatMap { case XformCase(pat, body) =>
        pat.matchAgainst(v).map(env => body.eval(env))
      }.headOption
    }
  
  /**
   * Create a constructor application from spec.
   */
  def construct(spec: LangSpec, conName: String, args: List[Val]): Option[Val] =
    spec.constructors.find(_.name == conName).map { con =>
      if args.length == con.arity then VCon(conName, args)
      else throw new Error(s"Constructor $conName expects ${con.arity} args, got ${args.length}")
    }

// End of Lang object
