package phi.phi.gen

import phi.phi.*

/** Validate: Semantic validation for phi.phi specs (generated from phi.phi)  */
object Validate:

  // ========== Validation Error ==========
  case class ValidationError(message: String, location: String)

  // ========== Collect declared names ==========
  def declaredSorts(spec: LangSpec): Set[String] = spec.sorts.map(_.name).toSet

  def declaredConstructors(spec: LangSpec): Set[String] = spec.constructors.map(_.name).toSet

  def declaredGrammars(spec: LangSpec): Set[String] = spec.grammars.keySet.toSet

  // ========== Collect referenced sorts ==========
  def referencedSorts(spec: LangSpec): Set[String] = spec.constructors.map(_.returnType).toSet ++ spec.constructors.flatMap(c => sortRefsInType(c.argTypes)).toSet ++ spec.xforms.flatMap(x => List(x.srcType, x.tgtType)).toSet

  def sortRefsInType(types: List[LangType]): List[String] = types.flatMap(sortRefsInSingleType)

  def sortRefsInSingleType(ty: LangType): List[String] = ty match
    case LangType.SortRef(name) => List(name)
    case LangType.Arrow(from, to) => sortRefsInSingleType(from) ++ sortRefsInSingleType(to)
    case LangType.ListOf(elem) => sortRefsInSingleType(elem)
    case LangType.Product(left, right) => sortRefsInSingleType(left) ++ sortRefsInSingleType(right)
    case LangType.TypeApp(name, args) => List(name) ++ sortRefsInType(args)
    case LangType.TypeVar(name) => Nil

  // ========== Collect referenced grammars ==========
  def referencedGrammars(spec: LangSpec): Set[String] = spec.grammars.values.flatMap(rules => rules.flatMap(r => nonTermsInRule(r))).toSet

  def nonTermsInRule(rule: SyntaxRule): List[String] = rule.pattern.collect({
  case SyntaxToken.NonTerm(name, _) => name })

  // ========== Main validation ==========
  def validate(spec: LangSpec): List[ValidationError] = {
    val declared = declaredSorts(spec)
    {
      val referenced = referencedSorts(spec)
      {
        val undeclaredSorts = referenced -- declared
        {
          val declGrammars = declaredGrammars(spec)
          {
            val refGrammars = referencedGrammars(spec)
            {
              val undeclaredGrammars = refGrammars -- declGrammars -- declared
              undeclaredSorts.toList.map(s => ValidationError("Undefined sort: " ++ s, "sort reference")) ++ undeclaredGrammars.toList.map(g => ValidationError("Undefined grammar/sort: " ++ g, "grammar non-terminal"))
            }
          }
        }
      }
    }
  }

  // ========== Convenience method ==========
  def isValid(spec: LangSpec): Boolean = validate(spec).isEmpty
