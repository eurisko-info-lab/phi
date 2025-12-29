package phi

import scala.util.parsing.combinator.*
import Val.*

/**
 * Minimal parser for .phi spec files
 */
object PhiParser extends RegexParsers:
  override def skipWhitespace = true
  override val whiteSpace = """(\s|//.*)+""".r
  
  // Tokens
  def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
  def string: Parser[String] = "\"" ~> """[^"]*""".r <~ "\""
  def arrow: Parser[String] = "→" | "->"
  def biArrow: Parser[String] = "⇄" | "<->"
  def mapsto: Parser[String] = "↦" | "|->"
  
  // Language spec
  def spec: Parser[LangSpec] = "language" ~> ident ~ ("{" ~> rep(decl) <~ "}") ^^ {
    case name ~ decls =>
      decls.foldLeft(LangSpec(name)) { (s, d) => d(s) }
  }
  
  type DeclFn = LangSpec => LangSpec
  
  def decl: Parser[DeclFn] = sortDecl | constructorDecl | grammarDecl | xformDecl | ruleDecl
  
  def sortDecl: Parser[DeclFn] = "sort" ~> ident ^^ { name => 
    s => s.copy(sorts = s.sorts :+ Sort(name))
  }
  
  def constructorDecl: Parser[DeclFn] = 
    "constructor" ~> ident ~ (":" ~> typeExpr) ^^ { case name ~ ty =>
      val (args, ret) = flattenArrow(ty)
      s => s.copy(constructors = s.constructors :+ Constructor(name, args, ret))
    }
  
  def typeExpr: Parser[LangType] = arrowType
  
  def arrowType: Parser[LangType] =
    simpleType ~ opt(arrow ~> arrowType) ^^ {
      case l ~ Some(r) => LangType.Arrow(l, r)
      case l ~ None => l
    }
  
  def simpleType: Parser[LangType] =
    ident ^^ { LangType.SortRef(_) } |
    "(" ~> typeExpr <~ ")"
  
  def flattenArrow(ty: LangType): (List[LangType], String) = ty match
    case LangType.Arrow(l, r) =>
      val (rest, ret) = flattenArrow(r)
      (l :: rest, ret)
    case LangType.SortRef(name) => (Nil, name)
    case _ => (Nil, "Unknown")
  
  def grammarDecl: Parser[DeclFn] =
    "grammar" ~> ident ~ ("{" ~> rep(grammarRule) <~ "}") ^^ { case name ~ rules =>
      s => s.copy(grammars = s.grammars + (name -> rules))
    }
  
  def grammarRule: Parser[SyntaxRule] =
    rep1(grammarToken) ~ ("=>" ~> syntaxArg) ^^ {
      case tokens ~ result =>
        SyntaxRule(tokens, result)
    }
  
  def grammarToken: Parser[SyntaxToken] =
    string ^^ { SyntaxToken.Literal(_) } |
    ident ^^ { name => SyntaxToken.NonTerm(name, None) }
  
  def syntaxArg: Parser[SyntaxArg] =
    ident ~ opt("(" ~> repsep(syntaxArg, ",") <~ ")") ^^ {
      case name ~ Some(args) => SyntaxArg.Con(name, args)
      case name ~ None => SyntaxArg.Ref(name)
    }
  
  def xformDecl: Parser[DeclFn] =
    "xform" ~> ident ~ (":" ~> ident) ~ (biArrow ~> ident) ^^ { case name ~ src ~ tgt =>
      s => s.copy(xforms = s.xforms :+ Xform(name, src, tgt))
    }
  
  def ruleDecl: Parser[DeclFn] =
    "rule" ~> qualifiedName ~ ("{" ~> ruleCase <~ "}") ^^ { case name ~ rc =>
      s => 
        val existing = s.rules.find(_.name == name)
        existing match
          case Some(r) => 
            s.copy(rules = s.rules.filterNot(_.name == name) :+ r.copy(cases = r.cases :+ rc))
          case None =>
            s.copy(rules = s.rules :+ Rule(name, List(rc)))
    }
  
  def qualifiedName: Parser[String] = ident ~ rep("." ~> ident) ^^ {
    case first ~ rest => (first :: rest).mkString(".")
  }
  
  def ruleCase: Parser[RuleCase] =
    pattern ~ (mapsto ~> pattern) ^^ { case lhs ~ rhs => RuleCase(lhs, rhs, None) }
  
  def pattern: Parser[MetaPattern] = appPattern
  
  def appPattern: Parser[MetaPattern] =
    atomPattern ~ rep(atomPattern) ^^ {
      case f ~ args => args.foldLeft(f)(MetaPattern.PApp(_, _))
    }
  
  def atomPattern: Parser[MetaPattern] =
    ident ~ opt("." ~> ident) ~ opt("(" ~> repsep(pattern, ",") <~ ")") ^^ {
      case name ~ Some(method) ~ Some(args) => 
        MetaPattern.PCon(s"$name.$method", args)
      case name ~ Some(method) ~ None =>
        MetaPattern.PCon(s"$name.$method", Nil)
      case name ~ None ~ Some(args) =>
        if name.head.isUpper || args.nonEmpty then MetaPattern.PCon(name, args)
        else MetaPattern.PVar(name)
      case name ~ None ~ None =>
        if name.head.isUpper then MetaPattern.PCon(name, Nil)
        else MetaPattern.PVar(name)
    } |
    "(" ~> pattern <~ ")" |
    string ^^ { s => MetaPattern.PCon(s, Nil) }

  def parseSpec(input: String): Either[String, LangSpec] =
    parseAll(spec, input) match
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg + " at " + failure.next.pos)

/** Load a spec from a file */
def parseSpecFromFile(path: String): Either[String, LangSpec] =
  val source = scala.io.Source.fromFile(path)
  try PhiParser.parseSpec(source.mkString)
  finally source.close()
