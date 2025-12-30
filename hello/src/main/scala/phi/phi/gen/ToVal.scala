package phi.phi.gen

import phi.phi.*
import phi.meta.Val
import phi.meta.Val.*

/** ToVal: Convert phi.phi types to Val representation (generated from phi.phi)  */
object ToVal:

  // ========== toCons helper ==========
  def toCons(list: List[Val]): Val = list match
    case Nil => VCon("Nil", Nil)
    case head :: tail => VCon("Cons", List(head, toCons(tail)))

  // ========== LangSpec.toVal ==========
  extension(spec: LangSpec)
    def toVal: Val = VCon("LangSpec", List(VStr(spec.name), toCons(spec.sorts.map(_.toVal)), toCons(spec.constructors.map(_.toVal)), spec.grammars.toVal, toCons(spec.xforms.map(_.toVal)), toCons(spec.rules.map(_.toVal))))

  // ========== Sort.toVal ==========
  extension(sort: Sort)
    def toVal: Val = VCon("Sort", List(VStr(sort.name)))

  // ========== Constructor.toVal ==========
  extension(con: Constructor)
    def toVal: Val = VCon("Constructor", List(VStr(con.name), toCons(con.argTypes.map(_.toVal)), VStr(con.returnType)))

  // ========== LangType.toVal ==========
  extension(ty: LangType)
    def toVal: Val = ty match
      case LangType.SortRef(name) => VCon("SortRef", List(VStr(name)))
      case LangType.Arrow(from, to) => VCon("Arrow", List(from.toVal, to.toVal))
      case LangType.ListOf(elem) => VCon("ListOf", List(elem.toVal))
      case LangType.Product(left, right) => VCon("Product", List(left.toVal, right.toVal))
      case LangType.TypeApp(name, args) => VCon("TypeApp", List(VStr(name), toCons(args.map(_.toVal))))
      case LangType.TypeVar(name) => VCon("TypeVar", List(VStr(name)))

  // ========== Grammar map toVal ==========
  extension(grammars: Map[String, List[SyntaxRule]])
    def toVal: Val = toCons(grammars.toList.map(kv => VCon("Grammar", List(VStr(kv._1), toCons(kv._2.map(_.toVal))))))

  // ========== SyntaxRule.toVal ==========
  extension(rule: SyntaxRule)
    def toVal: Val = VCon("SynRule", List(toCons(rule.pattern.map(_.toVal)), rule.result.toVal))

  // ========== SyntaxToken.toVal ==========
  extension(tok: SyntaxToken)
    def toVal: Val = tok match
      case SyntaxToken.Literal(text) => VCon("Literal", List(VStr(text)))
      case SyntaxToken.NonTerm(name, mod) => VCon("NonTerm", List(VStr(name), VStr(mod.getOrElse(""))))

  // ========== SyntaxArg.toVal ==========
  extension(arg: SyntaxArg)
    def toVal: Val = arg match
      case SyntaxArg.Ref(name) => VCon("ArgRef", List(VStr(name)))
      case SyntaxArg.Lit(value) => VCon("ArgLit", List(VStr(value)))
      case SyntaxArg.StrLit(value) => VCon("ArgStrLit", List(VStr(value)))
      case SyntaxArg.Wrap(wrapper, inner) => VCon("ArgWrap", List(VStr(wrapper), inner.toVal))
      case SyntaxArg.Con(name, args) => VCon("ArgCon", List(VStr(name), toCons(args.map(_.toVal))))
      case SyntaxArg.Hole => VCon("ArgHole", Nil)

  // ========== Xform.toVal ==========
  extension(xform: Xform)
    def toVal: Val = VCon("Xform", List(VStr(xform.name), toCons(xform.params.map(nt => VCon("Param", List(VStr(nt._1), VStr(nt._2))))), VStr(xform.srcType), VStr(xform.tgtType), toCons(xform.rules.map(_.toVal))))

  // ========== Rule.toVal ==========
  extension(rule: Rule)
    def toVal: Val = VCon("Rule", List(VStr(rule.name), toCons(rule.cases.map(_.toVal))))

  // ========== RuleCase.toVal ==========
  extension(rc: RuleCase)
    def toVal: Val = VCon("RuleCase", List(rc.pattern.toVal, rc.body.toVal))

  // ========== RuleGuard.toVal ==========
  extension(guard: RuleGuard)
    def toVal: Val = guard match
      case RuleGuard.IsConstructor(varName, conName) => VCon("IsConstructor", List(VStr(varName), VStr(conName)))
      case RuleGuard.Equals(left, right) => VCon("Equals", List(left.toVal, right.toVal))

  // ========== MetaPattern.toVal ==========
  extension(pat: MetaPattern)
    def toVal: Val = pat match
      case MetaPattern.PVar(name) => VCon("PVar", List(VStr(name)))
      case MetaPattern.PCon(name, args) => VCon("PCon", List(VStr(name), toCons(args.map(_.toVal))))
      case MetaPattern.PApp(func, arg) => VCon("PApp", List(func.toVal, arg.toVal))
      case MetaPattern.PSubst(body, varName, replacement) => VCon("PSubst", List(body.toVal, VStr(varName), replacement.toVal))
