package phi.core

import phi.meta.Term

/**
 * ESTIMATE: Zipper Navigation (~60 lines)
 * 
 * Zipper for navigating and editing Term structures.
 * Supports up/down navigation and hole filling.
 * 
 * This enables structured editing where you can:
 * - Navigate into child terms
 * - Navigate back to parent
 * - Fill holes at any position
 * - Replace focused term
 */
case class TermZipper[A](
  focus: Term[A],
  context: List[ZipperContext[A]] = Nil
):
  /** Move down into a Done term (for compound structures) */
  def down[B](extract: A => Term[B], rebuild: (A, Term[B]) => A): Option[TermZipper[B]] =
    focus match
      case Term.Done(a) =>
        val child = extract(a)
        Some(TermZipper(
          child, 
          ZipperContext.Parent(a, rebuild.asInstanceOf[(Any, Term[Any]) => Any]) 
            :: context.asInstanceOf[List[ZipperContext[Any]]]
        ).asInstanceOf[TermZipper[B]])
      case Term.Hole(_) => None

  /** Move up to parent context */
  def up: Option[TermZipper[?]] =
    context match
      case ZipperContext.Parent(parent, rebuild) :: rest =>
        val rebuilt = rebuild.asInstanceOf[(Any, Term[Any]) => Any](parent, focus.asInstanceOf[Term[Any]])
        val newFocus = Term.Done(rebuilt).asInstanceOf[Term[A]]
        Some(TermZipper(newFocus, rest.asInstanceOf[List[ZipperContext[A]]]))
      case Nil => None

  /** Fill the current hole with a value */
  def fill(value: A): TermZipper[A] =
    focus match
      case Term.Hole(_) => copy(focus = Term.Done(value))
      case _            => this

  /** Replace the current focus */
  def replace(newFocus: Term[A]): TermZipper[A] =
    copy(focus = newFocus)

  /** Navigate to root and return the complete term */
  def toTerm: Term[?] =
    up match
      case Some(parent) => parent.toTerm
      case None         => focus

  /** Check if at root */
  def isRoot: Boolean = context.isEmpty

  /** Check if focus is a hole */
  def isHole: Boolean = focus match
    case Term.Hole(_) => true
    case _ => false

enum ZipperContext[A]:
  case Parent[A, B](parent: B, rebuild: (B, Term[A]) => B) extends ZipperContext[A]

object TermZipper:
  /** Create a zipper focused on a term */
  def apply[A](term: Term[A]): TermZipper[A] = TermZipper(term, Nil)
