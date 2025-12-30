package phi.core

import java.security.MessageDigest
import java.nio.charset.StandardCharsets
import phi.meta.Term

/**
 * ESTIMATE: Hash Consing (~170 lines)
 * 
 * Content-addressed hash for terms (Unison-style).
 * Terms with the same structure have the same hash.
 * 
 * This enables:
 * - Canonical subterm sharing
 * - Hash-based equality
 * - Content-addressed storage
 */
opaque type Hash = String

object Hash:
  def apply(s: String): Hash = s

  def compute(data: String): Hash =
    val md = MessageDigest.getInstance("SHA-256")
    val bytes = md.digest(data.getBytes(StandardCharsets.UTF_8))
    bytes.map("%02x".format(_)).mkString.take(16) // Use first 16 hex chars

  def compute[A](term: Term[A]): Hash =
    term match
      case Term.Done(a) => compute(s"Done(${a.hashCode})")
      case Term.Hole(l) => compute(s"Hole(${l.getOrElse("")})")

  extension (h: Hash)
    def value: String = h
    def short: String = h.take(8)

  given Ordering[Hash] = Ordering.String.asInstanceOf[Ordering[Hash]]
  given CanEqual[Hash, Hash] = CanEqual.derived

/**
 * Type class for hashing terms of different types.
 */
trait TermHasher[A]:
  def hash(term: Term[A]): Hash
  def hashValue(a: A): Hash

object TermHasher:
  given TermHasher[Int] with
    def hash(term: Term[Int]): Hash = term match
      case Term.Done(n) => Hash.compute(s"Int:$n")
      case Term.Hole(l) => Hash.compute(s"Hole:${l.getOrElse("")}")
    def hashValue(n: Int): Hash = Hash.compute(s"Int:$n")

  given TermHasher[String] with
    def hash(term: Term[String]): Hash = term match
      case Term.Done(s) => Hash.compute(s"String:$s")
      case Term.Hole(l) => Hash.compute(s"Hole:${l.getOrElse("")}")
    def hashValue(s: String): Hash = Hash.compute(s"String:$s")

  given [A, B](using ha: TermHasher[A], hb: TermHasher[B]): TermHasher[(A, B)] with
    def hash(term: Term[(A, B)]): Hash = term match
      case Term.Done((a, b)) => Hash.compute(s"Pair:${ha.hashValue(a).value}:${hb.hashValue(b).value}")
      case Term.Hole(l) => Hash.compute(s"Hole:${l.getOrElse("")}")
    def hashValue(p: (A, B)): Hash = Hash.compute(s"Pair:${ha.hashValue(p._1).value}:${hb.hashValue(p._2).value}")

  given [A](using ha: TermHasher[A]): TermHasher[List[A]] with
    def hash(term: Term[List[A]]): Hash = term match
      case Term.Done(list) =>
        val hashes = list.map(ha.hashValue).map(_.value).mkString(",")
        Hash.compute(s"List:[$hashes]")
      case Term.Hole(l) => Hash.compute(s"Hole:${l.getOrElse("")}")
    def hashValue(list: List[A]): Hash =
      val hashes = list.map(ha.hashValue).map(_.value).mkString(",")
      Hash.compute(s"List:[$hashes]")

/**
 * Hash-consed term storage for canonical subterm sharing.
 */
class HashConsedStore[A](using hasher: TermHasher[A]):
  import scala.collection.mutable

  private val store: mutable.Map[Hash, Term[A]] = mutable.Map.empty
  private val reverseIndex: mutable.Map[Term[A], Hash] = mutable.Map.empty

  /** Intern a term, returning its canonical version */
  def intern(term: Term[A]): (Hash, Term[A]) =
    val hash = hasher.hash(term)
    store.get(hash) match
      case Some(existing) => (hash, existing)
      case None =>
        store(hash) = term
        reverseIndex(term) = hash
        (hash, term)

  /** Get hash for a term if it exists */
  def getHash(term: Term[A]): Option[Hash] =
    reverseIndex.get(term).orElse {
      val h = hasher.hash(term)
      if store.contains(h) then Some(h) else None
    }

  /** Get term by hash */
  def get(hash: Hash): Option[Term[A]] =
    store.get(hash)

  /** Check if hash exists */
  def contains(hash: Hash): Boolean =
    store.contains(hash)

  /** All stored hashes */
  def hashes: Set[Hash] =
    store.keySet.toSet

  /** Size of store */
  def size: Int = store.size

/**
 * Canonical term with embedded hash.
 */
case class Canonical[A](hash: Hash, term: Term[A]):
  def map[B](f: A => B)(using hasher: TermHasher[B]): Canonical[B] =
    val newTerm = term match
      case Term.Done(a) => Term.Done(f(a))
      case Term.Hole(l) => Term.Hole(l)
    Canonical(hasher.hash(newTerm), newTerm)

object Canonical:
  def apply[A](term: Term[A])(using hasher: TermHasher[A]): Canonical[A] =
    Canonical(hasher.hash(term), term)
