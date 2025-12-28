package phi

/**
 * Grammar specification AST for the Phi meta-language.
 */

// =============================================================================
// Grammar AST
// =============================================================================

/**
 * A complete grammar specification file.
 */
case class SpecFile(
  name: String,
  sections: List[Section]
)

/**
 * A section in a spec file.
 */
enum Section:
  case Grammar(sec: GrammarSec)
  case Imports(imports: List[Import])
  case Config(config: ConfigSec)

/**
 * Import statement.
 */
case class Import(path: String, alias: Option[String] = None)

/**
 * Configuration section.
 */
case class ConfigSec(entries: Map[String, String])

/**
 * Grammar section containing rules.
 */
case class GrammarSec(
  name: String,
  rules: List[GrammarRule]
)

/**
 * A grammar rule (production).
 */
case class GrammarRule(
  name: String,
  typeRef: Option[TypeRef],
  alternatives: List[Alternative],
  annotations: List[Annotation] = Nil
)

/**
 * An alternative in a grammar rule.
 */
case class Alternative(
  parts: List[GrammarPart],
  action: Option[String] = None,
  label: Option[String] = None
)

/**
 * A part of a grammar alternative.
 */
enum GrammarPart:
  case Terminal(value: String)
  case NonTerminal(name: String, binding: Option[String] = None)
  case Group(parts: List[GrammarPart])
  case Optional(part: GrammarPart)
  case Many(part: GrammarPart, atLeastOne: Boolean = false)
  case SepBy(part: GrammarPart, sep: GrammarPart)

/**
 * Modifier for grammar parts.
 */
enum Modifier:
  case Optional
  case Many
  case Many1
  case SepBy(sep: String)

/**
 * Annotation on grammar rules.
 */
case class Annotation(
  name: String,
  args: List[String] = Nil
)

/**
 * Type reference in grammar.
 */
enum TypeRef:
  case Simple(name: String)
  case Parameterized(name: String, params: List[TypeRef])
  case Function(from: TypeRef, to: TypeRef)
  case Tuple(elements: List[TypeRef])

// =============================================================================
// Expression Language
// =============================================================================

/**
 * Associativity for binary operators.
 */
enum Assoc:
  case Left
  case Right
  case NonAssoc

/**
 * Operator definition with precedence and associativity.
 */
case class OpDef(
  symbol: String,
  precedence: Int,
  assoc: Assoc,
  eval: (Int, Int) => Int
)

/**
 * Expression AST.
 */
enum Expr:
  case Num(value: Int)
  case Var(name: String)
  case BinOp(op: String, left: Expr, right: Expr)
  case UnaryOp(op: String, operand: Expr)
  case Paren(inner: Expr)
  case Call(func: String, args: List[Expr])
  case IfExpr(cond: Expr, thenBranch: Expr, elseBranch: Expr)
  case LetExpr(name: String, value: Expr, body: Expr)

object Expr:
  /** Standard arithmetic operators */
  val defaultOps: List[OpDef] = List(
    OpDef("+", 1, Assoc.Left, _ + _),
    OpDef("-", 1, Assoc.Left, _ - _),
    OpDef("*", 2, Assoc.Left, _ * _),
    OpDef("/", 2, Assoc.Left, (a, b) => if b != 0 then a / b else 0),
    OpDef("%", 2, Assoc.Left, (a, b) => if b != 0 then a % b else 0),
    OpDef("^", 3, Assoc.Right, (a, b) => math.pow(a, b).toInt)
  )

  val comparisonOps: List[OpDef] = List(
    OpDef("==", 0, Assoc.NonAssoc, (a, b) => if a == b then 1 else 0),
    OpDef("!=", 0, Assoc.NonAssoc, (a, b) => if a != b then 1 else 0),
    OpDef("<", 0, Assoc.NonAssoc, (a, b) => if a < b then 1 else 0),
    OpDef(">", 0, Assoc.NonAssoc, (a, b) => if a > b then 1 else 0),
    OpDef("<=", 0, Assoc.NonAssoc, (a, b) => if a <= b then 1 else 0),
    OpDef(">=", 0, Assoc.NonAssoc, (a, b) => if a >= b then 1 else 0)
  )

  val logicalOps: List[OpDef] = List(
    OpDef("&&", -1, Assoc.Left, (a, b) => if a != 0 && b != 0 then 1 else 0),
    OpDef("||", -2, Assoc.Left, (a, b) => if a != 0 || b != 0 then 1 else 0)
  )

  val allOps: List[OpDef] = defaultOps ++ comparisonOps ++ logicalOps

/**
 * Expression evaluator.
 */
object ExprEvaluator:
  type Env = Map[String, Int]

  def eval(expr: Expr, env: Env = Map.empty, ops: List[OpDef] = Expr.allOps): Int =
    expr match
      case Expr.Num(n) => n

      case Expr.Var(name) =>
        env.getOrElse(name, 0)

      case Expr.BinOp(op, left, right) =>
        val l = eval(left, env, ops)
        val r = eval(right, env, ops)
        ops.find(_.symbol == op).map(_.eval(l, r)).getOrElse(0)

      case Expr.UnaryOp(op, operand) =>
        val v = eval(operand, env, ops)
        op match
          case "-" => -v
          case "!" => if v == 0 then 1 else 0
          case _   => v

      case Expr.Paren(inner) =>
        eval(inner, env, ops)

      case Expr.Call(func, args) =>
        val argVals = args.map(eval(_, env, ops))
        func match
          case "abs"   => argVals.headOption.map(math.abs).getOrElse(0)
          case "min"   => argVals.reduceOption(_ min _).getOrElse(0)
          case "max"   => argVals.reduceOption(_ max _).getOrElse(0)
          case "sum"   => argVals.sum
          case _       => 0

      case Expr.IfExpr(cond, thenBr, elseBr) =>
        if eval(cond, env, ops) != 0 then eval(thenBr, env, ops)
        else eval(elseBr, env, ops)

      case Expr.LetExpr(name, value, body) =>
        val v = eval(value, env, ops)
        eval(body, env.updated(name, v), ops)

// =============================================================================
// Expression Parser with Precedence Climbing
// =============================================================================

/**
 * Expression parser using precedence climbing algorithm.
 */
object ExprParser:
  import Syntax.*

  /** Parse an expression */
  def expr(ops: List[OpDef] = Expr.allOps): Syntax[Expr] =
    new Syntax[Expr]:
      def parse(stream: TokenStream): ParseResult[Expr] =
        parseExpr(stream, ops, minPrec = -10)

      def render(term: Term[Expr]): Vector[Lex] = term match
        case Term.Done(e) => renderExpr(e)
        case Term.Hole(l) => Vector(Lex.HoleTok(l))

  private def parseExpr(stream: TokenStream, ops: List[OpDef], minPrec: Int): ParseResult[Expr] =
    val atomResult = parseAtom(stream.skipWs, ops)
    atomResult.term match
      case Term.Hole(l) => atomResult
      case Term.Done(left) =>
        parseBinOpRhs(atomResult.remaining, left, ops, minPrec)

  private def parseAtom(stream: TokenStream, ops: List[OpDef]): ParseResult[Expr] =
    stream.skipWs.peek match
      case Lex.IntLit(n) =>
        ParseResult.done(Expr.Num(n), stream.skipWs.advance)

      case Lex.Ident(name) =>
        val next = stream.skipWs.advance
        next.skipWs.peek match
          case Lex.Symbol("(") =>
            // Function call
            val argsResult = parseArgs(next.skipWs.advance, ops)
            argsResult.term match
              case Term.Done(args) => ParseResult.done(Expr.Call(name, args), argsResult.remaining)
              case Term.Hole(l) => ParseResult.hole(argsResult.remaining, l)
          case _ =>
            ParseResult.done(Expr.Var(name), next)

      case Lex.Symbol("(") =>
        val inner = parseExpr(stream.skipWs.advance, ops, -10)
        inner.term match
          case Term.Done(e) =>
            inner.remaining.skipWs.peek match
              case Lex.Symbol(")") =>
                ParseResult.done(Expr.Paren(e), inner.remaining.skipWs.advance)
              case _ =>
                ParseResult.hole(inner.remaining, Some("expected ')'"))
          case Term.Hole(l) => inner

      case Lex.Symbol("-") =>
        val operand = parseAtom(stream.skipWs.advance, ops)
        operand.map(e => Expr.UnaryOp("-", e))

      case Lex.Symbol("!") =>
        val operand = parseAtom(stream.skipWs.advance, ops)
        operand.map(e => Expr.UnaryOp("!", e))

      case Lex.Keyword("if") =>
        parseIfExpr(stream.skipWs.advance, ops)

      case Lex.Keyword("let") =>
        parseLetExpr(stream.skipWs.advance, ops)

      case Lex.HoleTok(l) =>
        ParseResult.hole(stream.skipWs.advance, l)

      case _ =>
        ParseResult.hole(stream, Some("expected expression"))

  private def parseBinOpRhs(stream: TokenStream, left: Expr, ops: List[OpDef], minPrec: Int): ParseResult[Expr] =
    stream.skipWs.peek match
      case Lex.Symbol(op) if ops.exists(_.symbol == op) =>
        val opDef = ops.find(_.symbol == op).get
        if opDef.precedence < minPrec then
          ParseResult.done(left, stream)
        else
          val nextMinPrec = opDef.assoc match
            case Assoc.Left     => opDef.precedence + 1
            case Assoc.Right    => opDef.precedence
            case Assoc.NonAssoc => opDef.precedence + 1

          val rightResult = parseExpr(stream.skipWs.advance, ops, nextMinPrec)
          rightResult.term match
            case Term.Done(right) =>
              val combined = Expr.BinOp(op, left, right)
              parseBinOpRhs(rightResult.remaining, combined, ops, minPrec)
            case Term.Hole(l) =>
              // Partial parse - return what we have
              ParseResult.done(Expr.BinOp(op, left, Expr.Var("?")), rightResult.remaining)

      case _ =>
        ParseResult.done(left, stream)

  private def parseArgs(stream: TokenStream, ops: List[OpDef]): ParseResult[List[Expr]] =
    stream.skipWs.peek match
      case Lex.Symbol(")") =>
        ParseResult.done(Nil, stream.skipWs.advance)
      case _ =>
        var args = List.empty[Expr]
        var current = stream
        var continue = true
        while continue do
          val argResult = parseExpr(current, ops, -10)
          argResult.term match
            case Term.Done(arg) =>
              args = args :+ arg
              argResult.remaining.skipWs.peek match
                case Lex.Symbol(",") =>
                  current = argResult.remaining.skipWs.advance
                case Lex.Symbol(")") =>
                  return ParseResult.done(args, argResult.remaining.skipWs.advance)
                case _ =>
                  return ParseResult.hole(argResult.remaining, Some("expected ',' or ')'"))
            case Term.Hole(l) =>
              return ParseResult.hole(argResult.remaining, l)
        ParseResult.done(args, current)

  private def parseIfExpr(stream: TokenStream, ops: List[OpDef]): ParseResult[Expr] =
    val condResult = parseExpr(stream, ops, -10)
    condResult.term match
      case Term.Done(cond) =>
        condResult.remaining.skipWs.peek match
          case Lex.Keyword("then") =>
            val thenResult = parseExpr(condResult.remaining.skipWs.advance, ops, -10)
            thenResult.term match
              case Term.Done(thenBr) =>
                thenResult.remaining.skipWs.peek match
                  case Lex.Keyword("else") =>
                    val elseResult = parseExpr(thenResult.remaining.skipWs.advance, ops, -10)
                    elseResult.map(elseBr => Expr.IfExpr(cond, thenBr, elseBr))
                  case _ =>
                    ParseResult.hole(thenResult.remaining, Some("expected 'else'"))
              case Term.Hole(l) => thenResult.asInstanceOf[ParseResult[Expr]]
          case _ =>
            ParseResult.hole(condResult.remaining, Some("expected 'then'"))
      case Term.Hole(l) => condResult.asInstanceOf[ParseResult[Expr]]

  private def parseLetExpr(stream: TokenStream, ops: List[OpDef]): ParseResult[Expr] =
    stream.skipWs.peek match
      case Lex.Ident(name) =>
        val afterName = stream.skipWs.advance
        afterName.skipWs.peek match
          case Lex.Symbol("=") =>
            val valueResult = parseExpr(afterName.skipWs.advance, ops, -10)
            valueResult.term match
              case Term.Done(value) =>
                valueResult.remaining.skipWs.peek match
                  case Lex.Keyword("in") =>
                    val bodyResult = parseExpr(valueResult.remaining.skipWs.advance, ops, -10)
                    bodyResult.map(body => Expr.LetExpr(name, value, body))
                  case _ =>
                    ParseResult.hole(valueResult.remaining, Some("expected 'in'"))
              case Term.Hole(l) => valueResult.asInstanceOf[ParseResult[Expr]]
          case _ =>
            ParseResult.hole(afterName, Some("expected '='"))
      case _ =>
        ParseResult.hole(stream, Some("expected identifier"))

  private def renderExpr(expr: Expr): Vector[Lex] = expr match
    case Expr.Num(n) =>
      Vector(Lex.IntLit(n))

    case Expr.Var(name) =>
      Vector(Lex.Ident(name))

    case Expr.BinOp(op, left, right) =>
      renderExpr(left) ++ Vector(Lex.Whitespace(" "), Lex.Symbol(op), Lex.Whitespace(" ")) ++ renderExpr(right)

    case Expr.UnaryOp(op, operand) =>
      Vector(Lex.Symbol(op)) ++ renderExpr(operand)

    case Expr.Paren(inner) =>
      Vector(Lex.Symbol("(")) ++ renderExpr(inner) ++ Vector(Lex.Symbol(")"))

    case Expr.Call(func, args) =>
      Vector(Lex.Ident(func), Lex.Symbol("(")) ++
        args.flatMap(a => renderExpr(a) :+ Lex.Symbol(",")).dropRight(1).toVector ++
        Vector(Lex.Symbol(")"))

    case Expr.IfExpr(cond, thenBr, elseBr) =>
      Vector(Lex.Keyword("if"), Lex.Whitespace(" ")) ++ renderExpr(cond) ++
        Vector(Lex.Whitespace(" "), Lex.Keyword("then"), Lex.Whitespace(" ")) ++ renderExpr(thenBr) ++
        Vector(Lex.Whitespace(" "), Lex.Keyword("else"), Lex.Whitespace(" ")) ++ renderExpr(elseBr)

    case Expr.LetExpr(name, value, body) =>
      Vector(Lex.Keyword("let"), Lex.Whitespace(" "), Lex.Ident(name), Lex.Whitespace(" "),
        Lex.Symbol("="), Lex.Whitespace(" ")) ++ renderExpr(value) ++
        Vector(Lex.Whitespace(" "), Lex.Keyword("in"), Lex.Whitespace(" ")) ++ renderExpr(body)

// =============================================================================
// Grammar Parsers
// =============================================================================

/**
 * Parser for grammar specifications.
 */
object GrammarParser:
  import Syntax.*

  /** Parse a type reference */
  val typeRef: Syntax[TypeRef] = delay {
    val simple = ident.iso(Iso(TypeRef.Simple.apply, {
      case TypeRef.Simple(n) => n
      case _ => "?"
    }))

    val parameterized = (ident ~ (symbol("<") *> typeRef.sepBy(symbol(",")) <* symbol(">"))).iso(Iso(
      { case (name, params) => TypeRef.Parameterized(name, params) },
      { case TypeRef.Parameterized(n, ps) => (n, ps); case _ => ("?", Nil) }
    ))

    parameterized | simple
  }

  /** Parse an annotation */
  val annotation: Syntax[Annotation] =
    (symbol("@") *> ident ~ (symbol("(") *> ident.sepBy(symbol(",")) <* symbol(")")).opt).iso(Iso(
      { case (name, args) => Annotation(name, args.getOrElse(Nil)) },
      { case Annotation(n, args) => (n, if args.isEmpty then None else Some(args)) }
    ))

  /** Parse a grammar part */
  val grammarPart: Syntax[GrammarPart] = delay {
    val terminal = stringLit.iso(Iso(
      GrammarPart.Terminal.apply,
      { case GrammarPart.Terminal(v) => v; case _ => "?" }
    ))

    val nonTerminal = (ident ~ (symbol(":") *> ident).opt).iso(Iso(
      { case (name, binding) => GrammarPart.NonTerminal(name, binding) },
      { case GrammarPart.NonTerminal(n, b) => (n, b); case _ => ("?", None) }
    ))

    val grouped = (symbol("(") *> grammarPart.many1 <* symbol(")")).iso(Iso(
      GrammarPart.Group.apply,
      { case GrammarPart.Group(ps) => ps; case _ => Nil }
    ))

    val base = terminal | grouped | nonTerminal

    // Postfix modifiers
    val modSyntax: Syntax[Option[String]] = new Syntax[Option[String]]:
      def parse(stream: TokenStream): ParseResult[Option[String]] =
        stream.skipWs.peek match
          case Lex.Symbol(s) if s == "?" || s == "*" || s == "+" =>
            ParseResult.done(Some(s), stream.skipWs.advance)
          case _ =>
            ParseResult.done(None, stream)
      def render(t: Term[Option[String]]): Vector[Lex] = t match
        case Term.Done(Some(s)) => Vector(Lex.Symbol(s))
        case _ => Vector.empty
    
    val withMod = (base ~ modSyntax).iso(Iso[(GrammarPart, Option[String]), GrammarPart](
      { case (part, mod) =>
        mod match
          case Some("?") => GrammarPart.Optional(part)
          case Some("*") => GrammarPart.Many(part, atLeastOne = false)
          case Some("+") => GrammarPart.Many(part, atLeastOne = true)
          case _ => part
      },
      {
        case GrammarPart.Optional(p) => (p, Some("?"))
        case GrammarPart.Many(p, false) => (p, Some("*"))
        case GrammarPart.Many(p, true) => (p, Some("+"))
        case p => (p, None)
      }
    ))

    withMod
  }

  /** Parse an alternative */
  val alternative: Syntax[Alternative] =
    (grammarPart.many ~ (symbol("->") *> ident).opt ~ (symbol("#") *> ident).opt).iso(Iso(
      { case ((parts, action), label) => Alternative(parts, action, label) },
      { case Alternative(ps, a, l) => ((ps, a), l) }
    ))

  /** Parse a grammar rule */
  val grammarRule: Syntax[GrammarRule] =
    (annotation.many ~ ident ~ (symbol(":") *> typeRef).opt ~ (symbol("=") *> alternative.sepBy(symbol("|")))).iso(Iso(
      { case (((anns, name), ty), alts) => GrammarRule(name, ty, alts, anns) },
      { case GrammarRule(n, ty, alts, anns) => (((anns, n), ty), alts) }
    ))

  /** Parse a grammar section */
  val grammarSec: Syntax[GrammarSec] =
    (keyword("grammar") *> ident ~ (symbol("{") *> grammarRule.many <* symbol("}"))).iso(Iso(
      { case (name, rules) => GrammarSec(name, rules) },
      { case GrammarSec(n, rs) => (n, rs) }
    ))

  /** Parse an import */
  val importStmt: Syntax[Import] =
    (keyword("import") *> stringLit ~ (keyword("as") *> ident).opt).iso(Iso(
      { case (path, alias) => Import(path, alias) },
      { case Import(p, a) => (p, a) }
    ))

  /** Parse config entry */
  val configEntry: Syntax[(String, String)] =
    (ident ~ (symbol("=") *> stringLit))

  /** Parse config section */
  val configSec: Syntax[ConfigSec] =
    (keyword("config") *> symbol("{") *> configEntry.many <* symbol("}")).iso(Iso(
      entries => ConfigSec(entries.toMap),
      { case ConfigSec(m) => m.toList }
    ))

  /** Parse a section */
  val section: Syntax[Section] = delay {
    val grammar = grammarSec.iso(Iso(Section.Grammar.apply, { case Section.Grammar(g) => g; case _ => GrammarSec("?", Nil) }))
    val imports = (keyword("imports") *> symbol("{") *> importStmt.many <* symbol("}")).iso(Iso(
      Section.Imports.apply,
      { case Section.Imports(is) => is; case _ => Nil }
    ))
    val config = configSec.iso(Iso(Section.Config.apply, { case Section.Config(c) => c; case _ => ConfigSec(Map.empty) }))

    grammar | imports | config
  }

  /** Parse a spec file */
  val specFile: Syntax[SpecFile] =
    (keyword("module") *> ident ~ section.many).iso(Iso(
      { case (name, sections) => SpecFile(name, sections) },
      { case SpecFile(n, ss) => (n, ss) }
    ))
