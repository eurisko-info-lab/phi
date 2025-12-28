package phi

import java.security.MessageDigest
import java.nio.charset.StandardCharsets
import scala.collection.immutable.{TreeMap, TreeSet}

/**
 * Content-addressed hash for terms (Unison-style).
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
 * Pijul-style repository with content-addressed storage,
 * patches, branches, and merge support.
 */
class Repo[A]:
  // Content-addressed term storage
  private var terms: Map[Hash, RepoEntry[A]] = Map.empty

  // Name to hash mapping
  private var nameIndex: Map[Name, Hash] = Map.empty

  // Patch storage
  private var patches: Map[Hash, Patch[A]] = Map.empty

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

  /** Apply a patch and update the current branch */
  def applyPatch(patch: Patch[A], term: Term[A]): Term[A] =
    // Store the patch
    patches = patches.updated(patch.id, patch)

    // Apply the change
    val newTerm = ChangeApplicator(patch.change, term)
    val newHash = store(newTerm)

    // Update branch
    val branch = branches(currentBranch)
    branches = branches.updated(currentBranch, branch.copy(
      head = Some(newHash),
      patches = patch.id :: branch.patches
    ))

    newTerm

  /** Revert a patch */
  def revertPatch(patchId: Hash, term: Term[A]): Option[Term[A]] =
    patches.get(patchId).map { patch =>
      val reverted = ChangeApplicator(patch.inverse, term)
      val newHash = store(reverted)

      val branch = branches(currentBranch)
      val revertPatch = Patch.create(s"Revert: ${patch.description}", patch.inverse, term)
      patches = patches.updated(revertPatch.id, revertPatch)
      branches = branches.updated(currentBranch, branch.copy(
        head = Some(newHash),
        patches = revertPatch.id :: branch.patches
      ))

      reverted
    }

  /** Get a patch by id */
  def getPatch(id: Hash): Option[Patch[A]] =
    patches.get(id)

  /** List patches for current branch */
  def listPatches: List[Patch[A]] =
    branches(currentBranch).patches.flatMap(patches.get)

  /** Create a new branch from current */
  def createBranch(name: String): Unit =
    val current = branches(currentBranch)
    branches = branches.updated(name, Branch(name, current.head, current.patches))

  /** Switch to a branch */
  def switchBranch(name: String): Boolean =
    if branches.contains(name) then
      currentBranch = name
      true
    else false

  /** Get current branch name */
  def getCurrentBranch: String = currentBranch

  /** List all branches */
  def listBranches: Set[String] = branches.keySet

  /** Get branch head */
  def getBranchHead(branchName: String): Option[Hash] =
    branches.get(branchName).flatMap(_.head)

  /** Get current head */
  def head: Option[Hash] =
    getBranchHead(currentBranch)

  /** Get current term */
  def currentTerm: Option[Term[A]] =
    head.flatMap(get)

  /**
   * Merge another branch into current.
   * Uses a simple strategy: apply all patches from source that aren't in target.
   */
  def merge(sourceBranch: String): MergeResult[A] =
    val source = branches.get(sourceBranch)
    val target = branches.get(currentBranch)

    (source, target) match
      case (Some(src), Some(tgt)) =>
        val sourcePatches = src.patches.toSet
        val targetPatches = tgt.patches.toSet
        val newPatches = sourcePatches -- targetPatches

        if newPatches.isEmpty then
          MergeResult.AlreadyUpToDate

        else
          // Check for conflicts (patches that modify same structure)
          val conflicts = findConflicts(newPatches.toList.flatMap(patches.get))

          if conflicts.nonEmpty then
            MergeResult.Conflict(conflicts)
          else
            // Apply new patches
            var current = tgt.head.flatMap(get).getOrElse(Term.hole[A])
            var appliedPatches = tgt.patches
            for
              patchId <- newPatches.toList.sortBy(patches.get(_).map(_.timestamp).getOrElse(0L))
              patch <- patches.get(patchId)
            do
              current = ChangeApplicator(patch.change, current)
              appliedPatches = patchId :: appliedPatches

            val newHash = store(current)
            branches = branches.updated(currentBranch, tgt.copy(
              head = Some(newHash),
              patches = appliedPatches
            ))
            MergeResult.Success(current)

      case _ => MergeResult.BranchNotFound

  /** Find conflicting patches (simplified: patches with overlapping changes) */
  private def findConflicts(patchList: List[Patch[A]]): List[(Patch[A], Patch[A])] =
    // Simplified conflict detection - in real impl would analyze change structure
    Nil // No conflicts detected for now

  /** Export repository state */
  def exportState: RepoState[A] =
    RepoState(
      terms = terms,
      nameIndex = nameIndex,
      patches = patches,
      branches = branches,
      currentBranch = currentBranch
    )

  /** Import repository state */
  def importState(state: RepoState[A]): Unit =
    terms = state.terms
    nameIndex = state.nameIndex
    patches = state.patches
    branches = state.branches
    currentBranch = state.currentBranch

/**
 * Merge result variants.
 */
enum MergeResult[+A]:
  case Success(term: Term[A])
  case AlreadyUpToDate
  case Conflict(conflicts: List[(Patch[A], Patch[A])])
  case BranchNotFound

/**
 * Serializable repository state.
 */
case class RepoState[A](
  terms: Map[Hash, RepoEntry[A]],
  nameIndex: Map[Name, Hash],
  patches: Map[Hash, Patch[A]],
  branches: Map[String, Branch],
  currentBranch: String
)

object Repo:
  def apply[A](): Repo[A] = new Repo[A]

  /** Create a repo with an initial term */
  def withInitial[A](term: Term[A], name: Name): Repo[A] =
    val repo = new Repo[A]
    repo.store(term, Set(name))
    val branch = repo.branches("main").copy(head = Some(repo.getHash(name).get))
    repo.branches = repo.branches.updated("main", branch)
    repo
