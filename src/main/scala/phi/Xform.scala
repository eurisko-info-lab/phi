package phi

/**
 * Reversible transformation between two term types.
 * Supports bidirectional transformations with hole propagation.
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

  /** Create from Iso */
  def fromIso[A, B](iso: Iso[A, B]): Xform[A, B] =
    new Xform[A, B]:
      def forward(term: Term[A]): Term[B] = term.map(iso.to)
      def backward(term: Term[B]): Term[A] = term.map(iso.from)

  /** Composed transformation */
  case class Composed[A, B, C](first: Xform[A, B], second: Xform[B, C]) extends Xform[A, C]:
    def forward(term: Term[A]): Term[C] = second.forward(first.forward(term))
    def backward(term: Term[C]): Term[A] = first.backward(second.backward(term))

  /** Inverted transformation */
  case class Inverted[A, B](inner: Xform[A, B]) extends Xform[B, A]:
    def forward(term: Term[B]): Term[A] = inner.backward(term)
    def backward(term: Term[A]): Term[B] = inner.forward(term)

// =============================================================================
// Xform Registry
// =============================================================================

/**
 * Registry for named transformations.
 */
object XformRegistry:
  private var registry: Map[String, Xform[?, ?]] = Map(
    "lc-to-ic" -> LCToIC,
    "typecheck" -> TypeChecker,
    "pattern-compile" -> PatternCompiler
  )

  def register[A, B](name: String, xform: Xform[A, B]): Unit =
    registry = registry.updated(name, xform)

  def get(name: String): Option[Xform[?, ?]] =
    registry.get(name)

  def list: Set[String] = registry.keySet
