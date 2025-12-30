package phi.phi

/**
 * LangSpec: Language specification parsed from .phi files
 */
case class LangSpec(
  name: String,
  sorts: List[Sort] = Nil,
  constructors: List[Constructor] = Nil,
  grammars: Map[String, List[SyntaxRule]] = Map.empty,
  rules: List[Rule] = Nil,
  xforms: List[Xform] = Nil
)

case class Sort(name: String)
case class Constructor(name: String, argTypes: List[LangType], returnType: String)
case class Rule(name: String, cases: List[RuleCase])
case class Xform(name: String, params: List[(String, String)], srcType: String, tgtType: String, rules: List[Rule] = Nil)

enum LangType:
  case SortRef(name: String)
  case Arrow(from: LangType, to: LangType)
  case ListOf(elem: LangType)
  case Product(left: LangType, right: LangType)
  case TypeApp(name: String, args: List[LangType])
  case TypeVar(name: String)

/** Grammar rule: tokens => Result(args) */
case class SyntaxRule(
  pattern: List[SyntaxToken],
  result: SyntaxArg
)

enum SyntaxToken:
  case Literal(text: String)
  case NonTerm(name: String, modifier: Option[String])

enum SyntaxArg:
  case Ref(name: String)
  case Lit(value: String)
  case StrLit(value: String)
  case Wrap(wrapper: String, inner: SyntaxArg)
  case Con(name: String, args: List[SyntaxArg])
  case Hole

/** Rule case: pattern ↦ result */
case class RuleCase(pattern: MetaPattern, body: MetaPattern, guard: Option[RuleGuard] = None)

enum RuleGuard:
  case IsConstructor(varName: String, conName: String)
  case Equals(left: MetaPattern, right: MetaPattern)

/** Meta-level patterns */
enum MetaPattern:
  case PVar(name: String)
  case PCon(name: String, args: List[MetaPattern])
  case PApp(func: MetaPattern, arg: MetaPattern)
  case PSubst(body: MetaPattern, varName: String, replacement: MetaPattern)
