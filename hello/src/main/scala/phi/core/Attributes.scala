package phi.core

import phi.meta.Term

/**
 * Attribute flow direction.
 * 
 * This makes explicit the attribute grammar that was implicit in:
 * - RenderCtx.indent (inherited attribute)
 * - Grammar result building (synthesized attribute)
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
  flow: FlowDirection,
  defaultValue: Option[Any] = None
)

/**
 * Attribute value attached to a node.
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
 * Flow-based attribute grammar builder.
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
        // Get inherited attrs as raw values
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
 * Standard attribute definitions used in Phi rendering.
 * Makes explicit what RenderCtx was doing implicitly.
 */
object StandardAttrs:
  val indent = AttrDef("indent", FlowDirection.Down, Some(0))
  val needsParens = AttrDef("needsParens", FlowDirection.Down, Some(false))
  val precedence = AttrDef("precedence", FlowDirection.Up, Some(0))
  
  /** Indent attribute grammar for rendering */
  def indentGrammar[A](getIndentDelta: A => Int): FlowAttrGrammar[A] =
    FlowAttrGrammar(
      inheritedFlows = List(
        InheritedFlow("indent", (node, attrs) =>
          val current = attrs.getOrElse("indent", 0).asInstanceOf[Int]
          Map("indent" -> (current + getIndentDelta(node)))
        )
      ),
      synthesizedFlows = Nil,
      attrDefs = List(indent)
    )
