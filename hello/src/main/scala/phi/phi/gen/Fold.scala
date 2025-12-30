package phi.phi.gen

import phi.phi.*

/** Fold: Generic fold/visitor for phi.phi AST types (generated from phi.phi)  */
trait Fold[A]:

  // ========== Combine results ==========
  def combine(a: A, b: A): A
  def empty: A

  // ========== Fold over LangSpec ==========
  def foldSpec(spec: LangSpec): A = List(spec.sorts.map(foldSort).foldLeft(empty)(combine), spec.constructors.map(foldConstructor).foldLeft(empty)(combine), spec.grammars.values.flatMap(rules => rules.map(foldSyntaxRule)).foldLeft(empty)(combine), spec.xforms.map(foldXform).foldLeft(empty)(combine), spec.rules.map(foldRule).foldLeft(empty)(combine)).foldLeft(empty)(combine)

  // ========== Sort ==========
  def foldSort(sort: Sort): A = empty

  // ========== Constructor ==========
  def foldConstructor(con: Constructor): A = con.argTypes.map(foldLangType).foldLeft(empty)(combine)

  // ========== LangType ==========
  def foldLangType(ty: LangType): A = ty match
    case LangType.SortRef(name) => empty
    case LangType.Arrow(from, to) => combine(foldLangType(from), foldLangType(to))
    case LangType.ListOf(elem) => foldLangType(elem)
    case LangType.Product(left, right) => combine(foldLangType(left), foldLangType(right))
    case LangType.TypeApp(name, args) => args.map(foldLangType).foldLeft(empty)(combine)
    case LangType.TypeVar(name) => empty

  // ========== SyntaxRule ==========
  def foldSyntaxRule(rule: SyntaxRule): A = combine(rule.pattern.map(foldSyntaxToken).foldLeft(empty)(combine), foldSyntaxArg(rule.result))

  // ========== SyntaxToken ==========
  def foldSyntaxToken(tok: SyntaxToken): A = empty

  // ========== SyntaxArg ==========
  def foldSyntaxArg(arg: SyntaxArg): A = arg match
    case SyntaxArg.Ref(name) => empty
    case SyntaxArg.Lit(value) => empty
    case SyntaxArg.StrLit(value) => empty
    case SyntaxArg.Wrap(wrapper, inner) => foldSyntaxArg(inner)
    case SyntaxArg.Con(name, args) => args.map(foldSyntaxArg).foldLeft(empty)(combine)
    case SyntaxArg.Hole => empty

  // ========== Xform ==========
  def foldXform(xform: Xform): A = xform.rules.map(foldRule).foldLeft(empty)(combine)

  // ========== Rule ==========
  def foldRule(rule: Rule): A = rule.cases.map(foldRuleCase).foldLeft(empty)(combine)

  // ========== RuleCase ==========
  def foldRuleCase(rc: RuleCase): A = combine(foldMetaPattern(rc.pattern), foldMetaPattern(rc.body))

  // ========== MetaPattern ==========
  def foldMetaPattern(pat: MetaPattern): A = pat match
    case MetaPattern.PVar(name) => empty
    case MetaPattern.PCon(name, args) => args.map(foldMetaPattern).foldLeft(empty)(combine)
    case MetaPattern.PApp(func, arg) => combine(foldMetaPattern(func), foldMetaPattern(arg))
    case MetaPattern.PSubst(body, varName, replacement) => combine(foldMetaPattern(body), foldMetaPattern(replacement))
