package phi

import scala.collection.immutable.ArraySeq

/**
 * Core Term representation with holes for incomplete/partial terms.
 * This is the foundation of the Phi structured editor.
 */
enum Term[+A]:
  /** A complete term with a value */
  case Done(value: A)
  /** An incomplete term (hole) with optional label */
  case Hole(label: Option[String] = None)

  def map[B](f: A => B): Term[B] = this match
    case Done(a)     => Done(f(a))
    case Hole(label) => Hole(label)

  def flatMap[B](f: A => Term[B]): Term[B] = this match
    case Done(a)     => f(a)
    case Hole(label) => Hole(label)

  def getOrElse[B >: A](default: => B): B = this match
    case Done(a) => a
    case Hole(_) => default

  def isDone: Boolean = this match
    case Done(_) => true
    case Hole(_) => false

  def isHole: Boolean = !isDone

  def toOption: Option[A] = this match
    case Done(a) => Some(a)
    case Hole(_) => None

object Term:
  def pure[A](a: A): Term[A] = Done(a)
  def hole[A]: Term[A] = Hole(None)
  def hole[A](label: String): Term[A] = Hole(Some(label))

  /** Sequence of terms - all must be Done for the result to be Done */
  def sequence[A](terms: Seq[Term[A]]): Term[Seq[A]] =
    terms.foldRight(Done(Seq.empty[A]): Term[Seq[A]]) { (ta, acc) =>
      for
        a  <- ta
        as <- acc
      yield a +: as
    }

  given [A]: CanEqual[Term[A], Term[A]] = CanEqual.derived

/**
 * Zipper for navigating and editing Term structures.
 * Supports up/down navigation and hole filling.
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
        Some(TermZipper(child, ZipperContext.Parent(a, rebuild.asInstanceOf[(Any, Term[Any]) => Any]) :: context.asInstanceOf[List[ZipperContext[Any]]]).asInstanceOf[TermZipper[B]])
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

enum ZipperContext[A]:
  case Parent[A, B](parent: B, rebuild: (B, Term[A]) => B) extends ZipperContext[A]

/**
 * Change represents structural edits that can be applied to terms.
 * Changes are composable and can be inverted.
 */
enum Change[A]:
  /** Insert a value into a hole */
  case Insert(value: A)
  /** Replace any term with a new one */
  case Replace(newTerm: Term[A])
  /** Create a hole (delete content) */
  case Delete(label: Option[String] = None)
  /** Map over the value if present */
  case Map[A, B](f: A => B, inv: B => A) extends Change[B]
  /** Sequence of changes applied in order */
  case Sequence(changes: List[Change[A]])
  /** Choice between changes (first successful one wins) */
  case Choice(changes: List[Change[A]])
  /** Apply change to all elements in a collection */
  case Many[A](change: Change[A]) extends Change[Seq[A]]

  /** Invert this change (for undo) */
  def invert(original: Term[A]): Change[A] = this match
    case Insert(_)    => Delete()
    case Replace(_)   => Replace(original)
    case Delete(_)    => original match
      case Term.Done(v) => Insert(v)
      case Term.Hole(l) => Delete(l)
    case Map(f, inv)  => Map(inv, f).asInstanceOf[Change[A]]
    case Sequence(cs) => Sequence(cs.reverse.map(_.invert(original)))
    case Choice(cs)   => Choice(cs.map(_.invert(original)))
    case Many(c)      => Many(c.invert(Term.hole)).asInstanceOf[Change[A]]

object Change:
  def insert[A](value: A): Change[A] = Insert(value)
  def replace[A](term: Term[A]): Change[A] = Replace(term)
  def delete[A]: Change[A] = Delete()
  def map[A, B](f: A => B, inv: B => A): Change[B] = Map(f, inv)
  def sequence[A](changes: Change[A]*): Change[A] = Sequence(changes.toList)
  def choice[A](changes: Change[A]*): Change[A] = Choice(changes.toList)
  def many[A](change: Change[A]): Change[Seq[A]] = Many(change)

/**
 * Apply changes to terms, producing new terms.
 */
object ChangeApplicator:
  def apply[A](change: Change[A], term: Term[A]): Term[A] = change match
    case Change.Insert(value) =>
      term match
        case Term.Hole(_) => Term.Done(value)
        case _            => term // Can only insert into holes

    case Change.Replace(newTerm) =>
      newTerm

    case Change.Delete(label) =>
      Term.Hole(label)

    case Change.Map(f, _) =>
      term.map(f.asInstanceOf[Any => A])

    case Change.Sequence(changes) =>
      changes.foldLeft(term)((t, c) => apply(c, t))

    case Change.Choice(changes) =>
      changes.view.map(c => apply(c, term)).find(_ != term).getOrElse(term)

    case Change.Many(innerChange) =>
      term match
        case Term.Done(seq: Seq[?]) =>
          val changed = seq.map(elem => 
            apply(innerChange.asInstanceOf[Change[Any]], Term.Done(elem))
          )
          Term.sequence(changed).asInstanceOf[Term[A]]
        case _ => term

/**
 * Editor state combining term and change history for undo/redo.
 */
case class Editor[A](
  current: Term[A],
  undoStack: List[(Term[A], Change[A])] = Nil,
  redoStack: List[(Term[A], Change[A])] = Nil
):
  def applyChange(change: Change[A]): Editor[A] =
    val newTerm = ChangeApplicator(change, current)
    Editor(
      current = newTerm,
      undoStack = (current, change) :: undoStack,
      redoStack = Nil
    )

  def undo: Option[Editor[A]] =
    undoStack match
      case (prevTerm, change) :: rest =>
        Some(Editor(
          current = prevTerm,
          undoStack = rest,
          redoStack = (current, change.invert(prevTerm)) :: redoStack
        ))
      case Nil => None

  def redo: Option[Editor[A]] =
    redoStack match
      case (nextTerm, change) :: rest =>
        Some(Editor(
          current = nextTerm,
          undoStack = (current, change.invert(nextTerm)) :: undoStack,
          redoStack = rest
        ))
      case Nil => None

object Editor:
  def apply[A](initial: Term[A]): Editor[A] = Editor(initial, Nil, Nil)
  def empty[A]: Editor[A] = Editor(Term.hole[A])
