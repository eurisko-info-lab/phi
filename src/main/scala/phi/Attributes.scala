package phi

/**
 * Second-order holes: Term[Term[A]] for dependent types.
 * Allows holes at both the value and type level.
 */
object DependentTypes:
  /** A term with a type that may itself be a hole */
  case class Typed[A](value: Term[A], ty: Term[LCType])

  /** Lift a simple term to a typed term with inferred type */
  def lift[A](term: Term[A], inferType: A => LCType): Typed[A] =
    term match
      case Term.Done(a) => Typed(term, Term.Done(inferType(a)))
      case Term.Hole(l) => Typed(term, Term.Hole(l.map(_ + "_type")))

  /** Apply a type-level function */
  def mapType[A](typed: Typed[A], f: LCType => LCType): Typed[A] =
    Typed(typed.value, typed.ty.map(f))

  /** Check if both value and type are complete */
  def isComplete[A](typed: Typed[A]): Boolean =
    typed.value.isDone && typed.ty.isDone

  /** Fill a value hole */
  def fillValue[A](typed: Typed[A], value: A): Typed[A] =
    typed.copy(value = Term.Done(value))

  /** Fill a type hole */
  def fillType[A](typed: Typed[A], ty: LCType): Typed[A] =
    typed.copy(ty = Term.Done(ty))

/**
 * Attribute flow direction.
 */
enum FlowDirection:
  case Down   // Inherited (from parent to child)
  case Up     // Synthesized (from child to parent)
  case Both   // Bidirectional

/**
 * An attribute definition.
 */
case class AttrDef(
  name: String,
  ty: LCType,
  flow: FlowDirection,
  defaultValue: Option[Any] = None
)

/**
 * Attribute value attached to a term.
 */
case class AttrValue(
  name: String,
  value: Term[Any]
)

/**
 * A term with attached attributes.
 */
case class Attributed[A](
  term: Term[A],
  inherited: Map[String, AttrValue] = Map.empty,
  synthesized: Map[String, AttrValue] = Map.empty
):
  /** Get an inherited attribute */
  def getInherited(name: String): Option[Term[Any]] =
    inherited.get(name).map(_.value)

  /** Get a synthesized attribute */
  def getSynthesized(name: String): Option[Term[Any]] =
    synthesized.get(name).map(_.value)

  /** Set an inherited attribute */
  def setInherited(name: String, value: Term[Any]): Attributed[A] =
    copy(inherited = inherited.updated(name, AttrValue(name, value)))

  /** Set a synthesized attribute */
  def setSynthesized(name: String, value: Term[Any]): Attributed[A] =
    copy(synthesized = synthesized.updated(name, AttrValue(name, value)))

  /** Map over the term while preserving attributes */
  def mapTerm[B](f: Term[A] => Term[B]): Attributed[B] =
    Attributed(f(term), inherited, synthesized)

object Attributed:
  def apply[A](term: Term[A]): Attributed[A] = Attributed(term, Map.empty, Map.empty)

/**
 * Attribute grammar for computing attributes.
 */
trait AttrGrammar[A]:
  /** Defined attributes */
  def attributes: List[AttrDef]

  /** Compute inherited attributes for children */
  def inheritedFor(parent: A, childIndex: Int, parentAttrs: Map[String, Any]): Map[String, Any]

  /** Compute synthesized attributes from children */
  def synthesizedFrom(node: A, childAttrs: List[Map[String, Any]]): Map[String, Any]

/**
 * Attribute evaluator that propagates attributes through a term tree.
 */
object AttrEvaluator:
  /** Evaluate attributes on a term tree */
  def evaluate[A](
    term: Attributed[A],
    grammar: AttrGrammar[A],
    getChildren: A => List[A]
  ): Attributed[A] =
    term.term match
      case Term.Done(a) =>
        // Get inherited attrs
        val inh = term.inherited.view.mapValues(_.value match {
          case Term.Done(v) => v
          case _ => null
        }).filter(_._2 != null).toMap

        // Recursively evaluate children
        val children = getChildren(a)
        val evaluatedChildren = children.zipWithIndex.map { case (child, idx) =>
          val childInh = grammar.inheritedFor(a, idx, inh)
          val childAttr = Attributed(
            Term.Done(child),
            childInh.map { case (k, v) => k -> AttrValue(k, Term.Done(v)) },
            Map.empty
          )
          evaluate(childAttr, grammar, getChildren)
        }

        // Compute synthesized attrs
        val childSynths = evaluatedChildren.map { child =>
          child.synthesized.view.mapValues(_.value match {
            case Term.Done(v) => v
            case _ => null
          }).filter(_._2 != null).toMap
        }
        val synth = grammar.synthesizedFrom(a, childSynths)
        val synthAttrs = synth.map { case (k, v) => k -> AttrValue(k, Term.Done(v)) }

        term.copy(synthesized = term.synthesized ++ synthAttrs)

      case Term.Hole(_) =>
        term // Can't evaluate holes

/**
 * Inherited flow: passes attributes from parent to children.
 */
case class InheritedFlow[A](
  name: String,
  compute: (A, Map[String, Any]) => Map[String, Any]
)

/**
 * Synthesized flow: aggregates attributes from children to parent.
 */
case class SynthesizedFlow[A](
  name: String,
  compute: (A, List[Map[String, Any]]) => Any
)

/**
 * Flow-based attribute grammar.
 */
class FlowAttrGrammar[A](
  inheritedFlows: List[InheritedFlow[A]],
  synthesizedFlows: List[SynthesizedFlow[A]],
  attrDefs: List[AttrDef]
) extends AttrGrammar[A]:

  def attributes: List[AttrDef] = attrDefs

  def inheritedFor(parent: A, childIndex: Int, parentAttrs: Map[String, Any]): Map[String, Any] =
    inheritedFlows.foldLeft(parentAttrs) { (attrs, flow) =>
      attrs ++ flow.compute(parent, attrs)
    }

  def synthesizedFrom(node: A, childAttrs: List[Map[String, Any]]): Map[String, Any] =
    synthesizedFlows.map { flow =>
      flow.name -> flow.compute(node, childAttrs)
    }.toMap

/**
 * Type checking with dependent types using attribute grammar.
 */
object DependentTypeChecker:
  /** Environment attribute (inherited) */
  val envAttr = AttrDef("env", LCType.TVar("Env"), FlowDirection.Down, Some(Map.empty))

  /** Type attribute (synthesized) */
  val typeAttr = AttrDef("type", LCType.TVar("Type"), FlowDirection.Up)

  /** Create attribute grammar for LC type checking */
  def grammar: FlowAttrGrammar[LC] =
    val inherited = List(
      InheritedFlow[LC]("env", (node, attrs) => node match
        case LC.Lam(param, _) =>
          val env = attrs.getOrElse("env", Map.empty).asInstanceOf[Map[String, LCType]]
          // Add parameter with fresh type variable
          Map("env" -> env.updated(param, LCType.TVar(s"t_$param")))
        case LC.Let(name, _, _) =>
          val env = attrs.getOrElse("env", Map.empty).asInstanceOf[Map[String, LCType]]
          // Will be updated when value is typed
          attrs
        case _ => attrs
      )
    )

    val synthesized = List(
      SynthesizedFlow[LC]("type", (node, childTypes) => node match
        case LC.Var(name) =>
          // Type comes from environment - need to look up
          LCType.TVar(s"var_$name")
        case LC.Lam(param, _) =>
          val paramTy = LCType.TVar(s"t_$param")
          val bodyTy = childTypes.headOption.flatMap(_.get("type")).getOrElse(LCType.TVar("?")).asInstanceOf[LCType]
          LCType.TArrow(paramTy, bodyTy)
        case LC.App(_, _) =>
          val funcTy = childTypes.headOption.flatMap(_.get("type")).getOrElse(LCType.TVar("?")).asInstanceOf[LCType]
          funcTy match
            case LCType.TArrow(_, to) => to
            case _ => LCType.TVar("?")
        case LC.Let(_, _, _) =>
          childTypes.lastOption.flatMap(_.get("type")).getOrElse(LCType.TVar("?")).asInstanceOf[LCType]
        case LC.Lit(_) =>
          LCType.TInt
        case LC.Prim(op, _) =>
          LCType.TVar(s"prim_$op")
      )
    )

    FlowAttrGrammar(inherited, synthesized, List(envAttr, typeAttr))

  /** Get children of an LC term */
  def lcChildren(lc: LC): List[LC] = lc match
    case LC.Var(_)         => Nil
    case LC.Lam(_, body)   => List(body)
    case LC.App(f, a)      => List(f, a)
    case LC.Let(_, v, b)   => List(v, b)
    case LC.Lit(_)         => Nil
    case LC.Prim(_, args)  => args

/**
 * Dependent pattern compilation with types.
 */
object DependentPatternCompiler:
  /** Compile a pattern match, tracking types through compilation */
  def compile(pm: PatternMatch, scrutineeTy: LCType): (LC, LCType) =
    val compiled = PatternCompiler.compile(pm)
    // Infer result type from cases
    val resultTy = pm.cases.headOption.map { mc =>
      inferBodyType(mc.body, Map.empty)
    }.getOrElse(LCType.TVar("?"))
    (compiled, resultTy)

  private def inferBodyType(lc: LC, env: Map[String, LCType]): LCType =
    TypeChecker.infer(lc, env) match
      case Right(typed) => typed.getType
      case Left(_)      => LCType.TVar("?")

  /** Compile dependent match to IC net with type annotations */
  def compileToIC(pm: PatternMatch, scrutineeTy: LCType): ICNet =
    val (compiled, _) = compile(pm, scrutineeTy)
    LCToIC.compile(compiled)
