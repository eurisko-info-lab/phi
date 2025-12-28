package phi

import java.security.MessageDigest
import java.nio.charset.StandardCharsets
import scala.collection.mutable

/**
 * Content hash for canonical term identification (Unison-style).
 * Terms with the same structure have the same hash.
 */
object ContentHash:
  /** Compute structural hash for any term */
  def hash[A](term: Term[A])(using hasher: TermHasher[A]): Hash =
    hasher.hash(term)

  /** Compute hash for raw data */
  def hashString(s: String): Hash =
    Hash.compute(s)

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

  given TermHasher[Boolean] with
    def hash(term: Term[Boolean]): Hash = term match
      case Term.Done(b) => Hash.compute(s"Bool:$b")
      case Term.Hole(l) => Hash.compute(s"Hole:${l.getOrElse("")}")
    def hashValue(b: Boolean): Hash = Hash.compute(s"Bool:$b")

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

  given [A](using ha: TermHasher[A]): TermHasher[Option[A]] with
    def hash(term: Term[Option[A]]): Hash = term match
      case Term.Done(Some(a)) => Hash.compute(s"Some:${ha.hashValue(a).value}")
      case Term.Done(None) => Hash.compute("None")
      case Term.Hole(l) => Hash.compute(s"Hole:${l.getOrElse("")}")
    def hashValue(opt: Option[A]): Hash = opt match
      case Some(a) => Hash.compute(s"Some:${ha.hashValue(a).value}")
      case None => Hash.compute("None")

/**
 * Hash-consed term storage for canonical subterm sharing.
 */
class HashConsedStore[A](using hasher: TermHasher[A]):
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
    val newTerm = term.map(f)
    Canonical(hasher.hash(newTerm), newTerm)

object Canonical:
  def apply[A](term: Term[A])(using hasher: TermHasher[A]): Canonical[A] =
    Canonical(hasher.hash(term), term)

/**
 * Content-addressed repository using hash-consing for sharing.
 */
class ContentAddressedRepo[A](using hasher: TermHasher[A]):
  private val store = HashConsedStore[A]()
  private val names: mutable.Map[Name, Hash] = mutable.Map.empty

  /** Store a term, returning its canonical form */
  def store(term: Term[A]): Canonical[A] =
    val (hash, canonical) = this.store.intern(term)
    Canonical(hash, canonical)

  /** Store with a name */
  def storeNamed(term: Term[A], name: Name): Canonical[A] =
    val can = store(term)
    names(name) = can.hash
    can

  /** Lookup by hash */
  def get(hash: Hash): Option[Term[A]] =
    store.get(hash)

  /** Lookup by name */
  def getByName(name: Name): Option[Canonical[A]] =
    names.get(name).flatMap(h => store.get(h).map(t => Canonical(h, t)))

  /** Check equality by hash */
  def equal(t1: Term[A], t2: Term[A]): Boolean =
    hasher.hash(t1) == hasher.hash(t2)

  /** Get all names */
  def allNames: Set[Name] = names.keySet.toSet

  /** Get all hashes */
  def allHashes: Set[Hash] = store.hashes

/**
 * Sharing-preserving change application.
 * Applies changes while maintaining hash-consing.
 */
object SharingChangeApplicator:
  def apply[A](change: Change[A], term: Term[A])(using hasher: TermHasher[A], store: HashConsedStore[A]): Canonical[A] =
    val result = ChangeApplicator(change, term)
    val (hash, canonical) = store.intern(result)
    Canonical(hash, canonical)
