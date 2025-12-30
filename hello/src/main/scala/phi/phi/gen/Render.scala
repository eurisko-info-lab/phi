package phi.phi.gen

import phi.phi.*

/** Render: Pretty printing for phi.phi AST types (generated from phi.phi)  */
object Render:

  // ========== LangSpec.show ==========
  extension(spec: LangSpec)
    def show: String = {
      val decls = spec.sorts.map(_.show).map(s => "  " ++ s) ++ spec.constructors.map(_.show).map(s => "  " ++ s) ++ spec.grammars.toList.map(g => showGrammar(g._1, g._2)).map(s => "  " ++ s) ++ spec.xforms.map(_.show).map(s => "  " ++ s) ++ spec.rules.map(_.show).map(s => "  " ++ s)
      "language " ++ spec.name ++ " {" ++ "\n" ++ decls.mkString("\n") ++ "\n" ++ "}"
    }

  // ========== Sort.show ==========
  extension(sort: Sort)
    def show: String = "sort " ++ sort.name

  // ========== Constructor.show ==========
  extension(con: Constructor)
    def show: String = {
      val argTypes = con.argTypes.map(_.show)
      {
        val typeStr = if argTypes.isEmpty then con.returnType else argTypes.mkString(" -> ") ++ " -> " ++ con.returnType
        "constructor " ++ con.name ++ " : " ++ typeStr
      }
    }

  // ========== LangType.show ==========
  extension(ty: LangType)
    def show: String = ty match
      case LangType.SortRef(name) => name
      case LangType.Arrow(from, to) => from.show ++ " -> " ++ to.show
      case LangType.ListOf(elem) => "List[" ++ elem.show ++ "]"
      case LangType.Product(left, right) => "(" ++ left.show ++ ", " ++ right.show ++ ")"
      case LangType.TypeApp(name, args) => name ++ "[" ++ args.map(_.show).mkString(", ") ++ "]"
      case LangType.TypeVar(name) => name

  // ========== Grammar helper ==========
  def showGrammar(name: String, rules: List[SyntaxRule]): String = "grammar " ++ name ++ " {" ++ "\n" ++ rules.map(r => "    " ++ r.show).mkString("\n") ++ "\n" ++ "  }"

  // ========== SyntaxRule.show ==========
  extension(rule: SyntaxRule)
    def show: String = {
      val pattern = rule.pattern.map(_.show).mkString(" ")
      pattern ++ " => " ++ rule.result.show
    }

  // ========== SyntaxToken.show ==========
  extension(tok: SyntaxToken)
    def show: String = tok match
      case SyntaxToken.Literal(text) => "\"" ++ text ++ "\""
      case SyntaxToken.NonTerm(name, None) => name
      case SyntaxToken.NonTerm(name, Some(mod)) => name ++ mod

  // ========== SyntaxArg.show ==========
  extension(arg: SyntaxArg)
    def show: String = arg match
      case SyntaxArg.Ref(name) => name
      case SyntaxArg.Lit(value) => value
      case SyntaxArg.StrLit(value) => "\"" ++ value ++ "\""
      case SyntaxArg.Wrap(wrapper, inner) => wrapper ++ "(" ++ inner.show ++ ")"
      case SyntaxArg.Con(name, Nil) => name
      case SyntaxArg.Con(name, args) => name ++ "(" ++ args.map(_.show).mkString(", ") ++ ")"
      case SyntaxArg.Hole => "?"

  // ========== Xform.show ==========
  extension(xform: Xform)
    def show: String = {
      val paramsStr = if xform.params.isEmpty then "" else "(" ++ xform.params.map(nt => nt._1 ++ ": " ++ nt._2).mkString(", ") ++ ")"
      "xform " ++ xform.name ++ paramsStr ++ " : " ++ xform.srcType ++ " <-> " ++ xform.tgtType
    }

  // ========== Rule.show ==========
  extension(rule: Rule)
    def show: String = rule.cases.map(rc => "rule " ++ rule.name ++ " {" ++ "\n" ++ "    " ++ rc.show ++ "\n" ++ "  }").mkString("\n")

  // ========== RuleCase.show ==========
  extension(rc: RuleCase)
    def show: String = {
      val guardStr = rc.guard.map(g => " | " ++ g.show)
      rc.pattern.show ++ guardStr.getOrElse("") ++ " |-> " ++ rc.body.show
    }

  // ========== RuleGuard.show ==========
  extension(guard: RuleGuard)
    def show: String = guard match
      case RuleGuard.IsConstructor(varName, conName) => varName ++ " is " ++ conName
      case RuleGuard.Equals(left, right) => left.show ++ " == " ++ right.show

  // ========== MetaPattern.show ==========
  extension(pat: MetaPattern)
    def show: String = pat match
      case MetaPattern.PVar(name) => name
      case MetaPattern.PCon(name, Nil) => name
      case MetaPattern.PCon(name, args) => name ++ "(" ++ args.map(_.show).mkString(", ") ++ ")"
      case MetaPattern.PApp(func, arg) => "(" ++ func.show ++ " " ++ arg.show ++ ")"
      case MetaPattern.PSubst(body, varName, replacement) => body.show ++ "[" ++ varName ++ " := " ++ replacement.show ++ "]"
