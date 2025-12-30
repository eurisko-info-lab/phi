package phi.core

import phi.meta.Term

/**
 * ESTIMATE: Change[A] System (~80 lines)
 * 
 * Change represents structural edits that can be applied to terms.
 * Changes are composable and can be inverted (for undo).
 * 
 * This enables:
 * - Insert values into holes
 * - Replace any term
 * - Delete content (create hole)
 * - Map over values
 * - Sequence multiple changes
 * - Choose between alternatives
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
    case Many(c)      => Many(c.invert(Term.Hole(None))).asInstanceOf[Change[A]]

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
      term match
        case Term.Done(a) => Term.Done(f.asInstanceOf[Any => Any](a).asInstanceOf[A])
        case hole @ Term.Hole(_) => hole.asInstanceOf[Term[A]]

    case Change.Sequence(changes) =>
      changes.foldLeft(term)((t, c) => apply(c, t))

    case Change.Choice(changes) =>
      changes.foldLeft(term) { (t, c) =>
        val result = apply(c, t)
        if result != t then return result
        t
      }

    case Change.Many(c) =>
      term match
        case Term.Done(seq: Seq[?]) =>
          val results = seq.map(a => apply(c.asInstanceOf[Change[Any]], Term.Done(a)))
          Term.sequence(results.toList).asInstanceOf[Term[A]]
        case _ => term

  extension [A](term: Term[A])
    def applyChange(change: Change[A]): Term[A] = ChangeApplicator(change, term)

// Helper for Term.sequence (from full phi)
extension [A](t: Term.type)
  def sequence(terms: List[Term[A]]): Term[List[A]] =
    terms.foldRight(Term.Done(List.empty[A]): Term[List[A]]) { (ta, acc) =>
      (ta, acc) match
        case (Term.Done(a), Term.Done(as)) => Term.Done(a :: as)
        case (Term.Hole(l), _) => Term.Hole(l)
        case (_, Term.Hole(l)) => Term.Hole(l)
    }
