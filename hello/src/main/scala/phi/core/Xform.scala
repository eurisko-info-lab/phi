package phi.core

import phi.meta.{Term, Iso}
import phi.core.map  // Import the map extension

/**
 * Reversible transformation between two term types.
 * Supports bidirectional transformations with hole propagation.
 * 
 * This is the explicit representation of what was implicit in MetaInterp rules.
 * Every xform declaration in .phi files corresponds to an Xform[A,B].
 */
trait Xform[A, B]:
  /** Transform A to B */
  def forward(term: Term[A]): Term[B]

  /** Transform B back to A */
  def backward(term: Term[B]): Term[A]

  /** Compose with another Xform */
  def andThen[C](other: Xform[B, C]): Xform[A, C] =
    Xform.Composed(this, other)

  /** Inverse transformation */
  def inverse: Xform[B, A] = Xform.Inverted(this)

  /** Apply forward and verify round-trip */
  def roundTrip(term: Term[A]): Boolean =
    val there = forward(term)
    val back = backward(there)
    term == back

object Xform:
  /** Identity transformation */
  def id[A]: Xform[A, A] = new Xform[A, A]:
    def forward(term: Term[A]): Term[A] = term
    def backward(term: Term[A]): Term[A] = term

  /** Create from functions */
  def apply[A, B](fwd: Term[A] => Term[B], bwd: Term[B] => Term[A]): Xform[A, B] =
    new Xform[A, B]:
      def forward(term: Term[A]): Term[B] = fwd(term)
      def backward(term: Term[B]): Term[A] = bwd(term)

  /** Create from Iso (for simple value-level transforms) */
  def fromIso[A, B](iso: Iso[A, B]): Xform[A, B] =
    new Xform[A, B]:
      def forward(term: Term[A]): Term[B] = term match
        case Term.Done(a) => Term.Done(iso.forward(a))
        case Term.Hole(l) => Term.Hole(l)
      def backward(term: Term[B]): Term[A] = term match
        case Term.Done(b) => Term.Done(iso.backward(b))
        case Term.Hole(l) => Term.Hole(l)

  // -------------------------------------------------------------------------
  // Profunctor operations - dimap, lmap, rmap
  // -------------------------------------------------------------------------
  
  /** Pre-compose: adapt input (contravariant) */
  def lmap[A0, A, B](xform: Xform[A, B])(f: A0 => A): Xform[A0, B] =
    new Xform[A0, B]:
      def forward(term: Term[A0]): Term[B] = xform.forward(term.map(f))
      def backward(term: Term[B]): Term[A0] = 
        // Can't reverse f without an inverse, so backward becomes partial
        Term.Hole(Some("lmap: no inverse for pre-composition"))
  
  /** Post-compose: adapt output (covariant) */
  def rmap[A, B, B0](xform: Xform[A, B])(g: B => B0): Xform[A, B0] =
    new Xform[A, B0]:
      def forward(term: Term[A]): Term[B0] = xform.forward(term).map(g)
      def backward(term: Term[B0]): Term[A] =
        Term.Hole(Some("rmap: no inverse for post-composition"))
  
  /** Profunctor dimap: both at once */
  def dimap[A0, A, B, B0](xform: Xform[A, B])(f: A0 => A)(g: B => B0): Xform[A0, B0] =
    rmap(lmap(xform)(f))(g)
  
  /** With inverses: full round-trip support */
  def dimapIso[A0, A, B, B0](xform: Xform[A, B])(f: Iso[A0, A])(g: Iso[B, B0]): Xform[A0, B0] =
    new Xform[A0, B0]:
      def forward(term: Term[A0]): Term[B0] = 
        xform.forward(term.map(f.forward)).map(g.forward)
      def backward(term: Term[B0]): Term[A0] = 
        xform.backward(term.map(g.backward)).map(f.backward)

  /** Composed transformation */
  case class Composed[A, B, C](first: Xform[A, B], second: Xform[B, C]) extends Xform[A, C]:
    def forward(term: Term[A]): Term[C] = second.forward(first.forward(term))
    def backward(term: Term[C]): Term[A] = first.backward(second.backward(term))

  /** Inverted transformation */
  case class Inverted[A, B](inner: Xform[A, B]) extends Xform[B, A]:
    def forward(term: Term[B]): Term[A] = inner.backward(term)
    def backward(term: Term[A]): Term[B] = inner.forward(term)

/**
 * Registry for named transformations.
 * Maps xform names from .phi files to their implementations.
 */
object XformRegistry:
  private var registry: Map[String, Xform[?, ?]] = Map.empty

  def register[A, B](name: String, xform: Xform[A, B]): Unit =
    registry = registry.updated(name, xform)

  def get(name: String): Option[Xform[?, ?]] =
    registry.get(name)

  def list: Set[String] = registry.keySet

  /** Create an Xform from a LangSpec's rules */
  def fromSpec(spec: phi.phi.LangSpec, xformName: String): Option[Xform[phi.meta.Val, phi.meta.Val]] =
    val interp = phi.meta.LangInterpreter(spec)
    Some(new Xform[phi.meta.Val, phi.meta.Val]:
      def forward(term: Term[phi.meta.Val]): Term[phi.meta.Val] = term match
        case Term.Done(v) => 
          interp.applyXform(xformName, v) match
            case Some(result) => Term.Done(result)
            case None => Term.Hole(Some(s"$xformName failed"))
        case Term.Hole(l) => Term.Hole(l)
      
      def backward(term: Term[phi.meta.Val]): Term[phi.meta.Val] =
        // For now, backward requires explicit inverse rules
        // TODO: derive backward from forward rules when possible
        Term.Hole(Some(s"$xformName.backward not implemented"))
    )
