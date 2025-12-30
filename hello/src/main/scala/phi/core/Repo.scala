package phi.core

import phi.meta.Term
import scala.collection.mutable

/**
 * ESTIMATE: Repo/Patches (~320 lines)
 * 
 * Pijul-style repository with content-addressed storage,
 * patches, branches, and merge support.
 * 
 * This enables:
 * - Version control for terms
 * - Named references
 * - Patch-based history
 * - Branch management
 * - Invertible changes
 */

/**
 * A patch represents a change with its inverse for Pijul-style versioning.
 */
case class Patch[A](
  id: Hash,
  description: String,
  change: Change[A],
  inverse: Change[A],
  dependencies: Set[Hash] = Set.empty,
  timestamp: Long = System.currentTimeMillis()
):
  /** Check if this patch depends on another */
  def dependsOn(other: Hash): Boolean = dependencies.contains(other)

object Patch:
  def create[A](description: String, change: Change[A], original: Term[A]): Patch[A] =
    val inverse = change.invert(original)
    val id = Hash.compute(s"$description-${System.nanoTime()}")
    Patch(id, description, change, inverse)

  def createWithDeps[A](
    description: String,
    change: Change[A],
    original: Term[A],
    deps: Set[Hash]
  ): Patch[A] =
    val inverse = change.invert(original)
    val id = Hash.compute(s"$description-${System.nanoTime()}")
    Patch(id, description, change, inverse, deps)

/**
 * A named reference to a term in the repository.
 */
case class Name(path: List[String]):
  def /(segment: String): Name = Name(path :+ segment)
  override def toString: String = path.mkString(".")

object Name:
  def apply(s: String): Name = Name(s.split("\\.").toList)
  val root: Name = Name(Nil)

/**
 * Entry in the repository: a term with its hash and metadata.
 */
case class RepoEntry[A](
  hash: Hash,
  term: Term[A],
  names: Set[Name] = Set.empty,
  metadata: Map[String, String] = Map.empty
)

/**
 * Branch state: head hash and applied patches.
 */
case class Branch(
  name: String,
  head: Option[Hash],
  patches: List[Hash] = Nil
):
  def isEmpty: Boolean = head.isEmpty

object Branch:
  def empty(name: String): Branch = Branch(name, None, Nil)

/**
 * Pijul-style repository with content-addressed storage.
 */
class Repo[A]:
  // Content-addressed term storage
  private var terms: Map[Hash, RepoEntry[A]] = Map.empty

  // Name to hash mapping
  private var nameIndex: Map[Name, Hash] = Map.empty

  // Patch storage
  private var patchStore: Map[Hash, Patch[A]] = Map.empty

  // Branch storage
  private var branches: Map[String, Branch] = Map("main" -> Branch.empty("main"))

  // Current branch
  private var currentBranch: String = "main"

  /** Store a term and return its hash */
  def store(term: Term[A], names: Set[Name] = Set.empty): Hash =
    val hash = Hash.compute(term)
    val existing = terms.get(hash)
    val entry = existing match
      case Some(e) => e.copy(names = e.names ++ names)
      case None    => RepoEntry(hash, term, names)
    terms = terms.updated(hash, entry)
    names.foreach(n => nameIndex = nameIndex.updated(n, hash))
    hash

  /** Retrieve a term by hash */
  def get(hash: Hash): Option[Term[A]] =
    terms.get(hash).map(_.term)

  /** Retrieve a term by name */
  def getByName(name: Name): Option[Term[A]] =
    nameIndex.get(name).flatMap(get)

  /** Get hash by name */
  def getHash(name: Name): Option[Hash] =
    nameIndex.get(name)

  /** Get entry by hash */
  def getEntry(hash: Hash): Option[RepoEntry[A]] =
    terms.get(hash)

  /** List all stored hashes */
  def listHashes: Set[Hash] =
    terms.keySet

  /** List all names */
  def listNames: Set[Name] =
    nameIndex.keySet

  // ===========================================================================
  // Patch Operations
  // ===========================================================================

  /** Apply a patch to a term */
  def applyPatch(patch: Patch[A], term: Term[A]): Term[A] =
    patchStore = patchStore.updated(patch.id, patch)
    // Update current branch
    branches.get(currentBranch).foreach { branch =>
      branches = branches.updated(currentBranch, 
        branch.copy(patches = branch.patches :+ patch.id))
    }
    ChangeApplicator(patch.change, term)

  /** Unapply (revert) a patch */
  def revertPatch(patch: Patch[A], term: Term[A]): Term[A] =
    ChangeApplicator(patch.inverse, term)

  /** Get patch by ID */
  def getPatch(id: Hash): Option[Patch[A]] =
    patchStore.get(id)

  /** List all patches */
  def listPatches: List[Patch[A]] =
    patchStore.values.toList.sortBy(_.timestamp)

  // ===========================================================================
  // Branch Operations
  // ===========================================================================

  /** Create a new branch */
  def createBranch(name: String): Unit =
    branches.get(currentBranch).foreach { current =>
      branches = branches.updated(name, Branch(name, current.head, current.patches))
    }

  /** Switch to a branch */
  def checkout(name: String): Boolean =
    if branches.contains(name) then
      currentBranch = name
      true
    else false

  /** Get current branch name */
  def getCurrentBranch: String = currentBranch

  /** List all branches */
  def listBranches: Set[String] = branches.keySet

  /** Update branch head */
  def updateHead(hash: Hash): Unit =
    branches.get(currentBranch).foreach { branch =>
      branches = branches.updated(currentBranch, branch.copy(head = Some(hash)))
    }

  /** Get current head */
  def head: Option[Hash] =
    branches.get(currentBranch).flatMap(_.head)

  // ===========================================================================
  // Statistics
  // ===========================================================================

  def stats: Map[String, Int] = Map(
    "terms" -> terms.size,
    "names" -> nameIndex.size,
    "patches" -> patchStore.size,
    "branches" -> branches.size
  )
