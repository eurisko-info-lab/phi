package phi

import cats.effect.*
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.*
import org.http4s.implicits.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.`Content-Type`
import org.http4s.Charset
import io.circe.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import com.comcast.ip4s.*
import scala.collection.mutable

// =============================================================================
// JSON Codecs for Phi types
// =============================================================================

object PhiCodecs:
  // Term codecs
  given termEncoder[A: Encoder]: Encoder[Term[A]] = Encoder.instance {
    case Term.Done(a) => Json.obj("type" -> "done".asJson, "value" -> a.asJson)
    case Term.Hole(l) => Json.obj("type" -> "hole".asJson, "label" -> l.asJson)
  }

  given termDecoder[A: Decoder]: Decoder[Term[A]] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "done" => c.get[A]("value").map(Term.Done(_))
      case "hole" => c.get[Option[String]]("label").map(Term.Hole(_))
      case other  => Left(DecodingFailure(s"Unknown term type: $other", c.history))
    }
  }

  // Hash codec
  given Encoder[Hash] = Encoder.encodeString.contramap(_.value)
  given Decoder[Hash] = Decoder.decodeString.map(Hash.apply)

  // Name codec
  given Encoder[Name] = Encoder.encodeString.contramap(_.toString)
  given Decoder[Name] = Decoder.decodeString.map(Name.apply)

  // LC codec
  given Encoder[LC] = Encoder.instance {
    case LC.Var(n)        => Json.obj("type" -> "var".asJson, "name" -> n.asJson)
    case LC.Lam(p, b)     => Json.obj("type" -> "lam".asJson, "param" -> p.asJson, "body" -> b.asJson)
    case LC.App(f, a)     => Json.obj("type" -> "app".asJson, "func" -> f.asJson, "arg" -> a.asJson)
    case LC.Let(n, v, b)  => Json.obj("type" -> "let".asJson, "name" -> n.asJson, "value" -> v.asJson, "body" -> b.asJson)
    case LC.Lit(v)        => Json.obj("type" -> "lit".asJson, "value" -> v.asJson)
    case LC.Prim(op, as)  => Json.obj("type" -> "prim".asJson, "op" -> op.asJson, "args" -> as.asJson)
  }

  given Decoder[LC] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "var"  => c.get[String]("name").map(LC.Var.apply)
      case "lam"  => for { p <- c.get[String]("param"); b <- c.get[LC]("body") } yield LC.Lam(p, b)
      case "app"  => for { f <- c.get[LC]("func"); a <- c.get[LC]("arg") } yield LC.App(f, a)
      case "let"  => for { n <- c.get[String]("name"); v <- c.get[LC]("value"); b <- c.get[LC]("body") } yield LC.Let(n, v, b)
      case "lit"  => c.get[Int]("value").map(LC.Lit.apply)
      case "prim" => for { op <- c.get[String]("op"); as <- c.get[List[LC]]("args") } yield LC.Prim(op, as)
      case other  => Left(DecodingFailure(s"Unknown LC type: $other", c.history))
    }
  }

  // Expr codec
  given Encoder[Expr] = Encoder.instance {
    case Expr.Num(n)             => Json.obj("type" -> "num".asJson, "value" -> n.asJson)
    case Expr.Var(n)             => Json.obj("type" -> "var".asJson, "name" -> n.asJson)
    case Expr.BinOp(op, l, r)    => Json.obj("type" -> "binop".asJson, "op" -> op.asJson, "left" -> l.asJson, "right" -> r.asJson)
    case Expr.UnaryOp(op, e)     => Json.obj("type" -> "unary".asJson, "op" -> op.asJson, "operand" -> e.asJson)
    case Expr.Paren(e)           => Json.obj("type" -> "paren".asJson, "inner" -> e.asJson)
    case Expr.Call(f, as)        => Json.obj("type" -> "call".asJson, "func" -> f.asJson, "args" -> as.asJson)
    case Expr.IfExpr(c, t, e)    => Json.obj("type" -> "if".asJson, "cond" -> c.asJson, "then" -> t.asJson, "else" -> e.asJson)
    case Expr.LetExpr(n, v, b)   => Json.obj("type" -> "let".asJson, "name" -> n.asJson, "value" -> v.asJson, "body" -> b.asJson)
  }

  given Decoder[Expr] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "num"   => c.get[Int]("value").map(Expr.Num.apply)
      case "var"   => c.get[String]("name").map(Expr.Var.apply)
      case "binop" => for { op <- c.get[String]("op"); l <- c.get[Expr]("left"); r <- c.get[Expr]("right") } yield Expr.BinOp(op, l, r)
      case "unary" => for { op <- c.get[String]("op"); e <- c.get[Expr]("operand") } yield Expr.UnaryOp(op, e)
      case "paren" => c.get[Expr]("inner").map(Expr.Paren.apply)
      case "call"  => for { f <- c.get[String]("func"); as <- c.get[List[Expr]]("args") } yield Expr.Call(f, as)
      case "if"    => for { co <- c.get[Expr]("cond"); t <- c.get[Expr]("then"); e <- c.get[Expr]("else") } yield Expr.IfExpr(co, t, e)
      case "let"   => for { n <- c.get[String]("name"); v <- c.get[Expr]("value"); b <- c.get[Expr]("body") } yield Expr.LetExpr(n, v, b)
      case other   => Left(DecodingFailure(s"Unknown Expr type: $other", c.history))
    }
  }

// =============================================================================
// Server State
// =============================================================================

/**
 * Shared server state containing repositories and editors.
 */
class ServerState:
  val lcRepo: Repo[LC] = Repo[LC]()
  val exprRepo: Repo[Expr] = Repo[Expr]()
  val editors: mutable.Map[String, Editor[?]] = mutable.Map.empty

  // Initialize repository with standard combinators
  initializeRepo()

  def getOrCreateLCEditor(id: String): Editor[LC] =
    editors.getOrElseUpdate(id, Editor.empty[LC]).asInstanceOf[Editor[LC]]

  def getOrCreateExprEditor(id: String): Editor[Expr] =
    editors.getOrElseUpdate(id, Editor.empty[Expr]).asInstanceOf[Editor[Expr]]

  /** Initialize the LC repository with standard combinators and examples */
  private def initializeRepo(): Unit =
    val standardLibrary: List[(String, String, String)] = List(
      // Identity and basic combinators
      ("id", "fn x => x", "Identity combinator"),
      ("const", "fn x => fn y => x", "Constant combinator (K)"),
      ("flip", "fn f => fn x => fn y => f y x", "Flip arguments"),
      ("compose", "fn f => fn g => fn x => f (g x)", "Function composition"),
      
      // Boolean Church encodings
      ("true", "fn t => fn f => t", "Church true"),
      ("false", "fn t => fn f => f", "Church false"),
      ("not", "fn b => fn t => fn f => b f t", "Boolean negation"),
      ("and", "fn p => fn q => p q p", "Boolean and"),
      ("or", "fn p => fn q => p p q", "Boolean or"),
      ("if", "fn c => fn t => fn e => c t e", "If-then-else"),
      
      // Pairs
      ("pair", "fn a => fn b => fn f => f a b", "Construct a pair"),
      ("fst", "fn p => p (fn a => fn b => a)", "First of pair"),
      ("snd", "fn p => p (fn a => fn b => b)", "Second of pair"),
      
      // Church numerals
      ("zero", "fn f => fn x => x", "Church numeral 0"),
      ("one", "fn f => fn x => f x", "Church numeral 1"),
      ("two", "fn f => fn x => f (f x)", "Church numeral 2"),
      ("three", "fn f => fn x => f (f (f x))", "Church numeral 3"),
      ("succ", "fn n => fn f => fn x => f (n f x)", "Successor"),
      ("plus", "fn m => fn n => fn f => fn x => m f (n f x)", "Addition"),
      ("mult", "fn m => fn n => fn f => m (n f)", "Multiplication"),
      
      // Recursion
      ("omega", "fn x => x x", "Self-application"),
      ("Y", "fn f => (fn x => f (x x)) (fn x => f (x x))", "Y combinator (fixed-point)"),
      
      // Practical examples
      ("double", "fn x => x + x", "Double a number"),
      ("square", "fn x => x * x", "Square a number"),
      ("add", "fn x => fn y => x + y", "Add two numbers"),
      ("mul", "fn x => fn y => x * y", "Multiply two numbers"),
      
      // Example expressions
      ("example1", "let x = 5 in x * 2 + 1", "Let binding example"),
      ("example2", "let square = fn x => x * x in square 4", "Function in let"),
      ("example3", "let add = fn x => fn y => x + y in add 3 4", "Curried addition"),
      ("example4", "(fn f => fn x => f (f x)) (fn y => y + 1) 0", "Apply twice")
    )

    // Store each term with a patch for history
    var lastTerm: Term[LC] = Term.hole[LC]
    
    for (name, code, description) <- standardLibrary do
      Pipeline.parseLC(code) match
        case Right(term) =>
          // Store with name
          lcRepo.store(term, Set(Name(name)))
          
          // Create a patch for history
          val change = Change.replace[LC](term)
          val patch = Patch.create(s"Add $name: $description", change, lastTerm)
          lcRepo.applyPatch(patch, lastTerm)
          lastTerm = term
          
        case Left(err) =>
          System.err.println(s"Failed to parse $name: $err")

// =============================================================================
// API Response Types
// =============================================================================

case class TermResponse[A](
  hash: String,
  term: Term[A],
  isComplete: Boolean
)

case class RepoInfo(
  branches: List[String],
  currentBranch: String,
  head: Option[String],
  termCount: Int,
  names: List[String]
)

case class EditorState(
  id: String,
  termType: String,
  isComplete: Boolean,
  canUndo: Boolean,
  canRedo: Boolean
)

case class ParseRequest(input: String)
case class EvalRequest(input: String, env: Map[String, Int])
case class ChangeRequest(changeType: String, value: Option[Json])
case class XformRequest(xformName: String, termHash: String)
case class CommitRequest(code: String, message: String, name: Option[String] = None)
case class BranchRequest(name: String)

// =============================================================================
// HTTP Routes
// =============================================================================

object PhiRoutes:
  import PhiCodecs.given

  def routes(state: ServerState): HttpRoutes[IO] = HttpRoutes.of[IO] {

    // =========================================================================
    // Health & Info
    // =========================================================================

    case GET -> Root / "health" =>
      Ok(Json.obj("status" -> "ok".asJson))

    case GET -> Root / "info" =>
      Ok(Json.obj(
        "name" -> "Phi Language Server".asJson,
        "version" -> "0.1.0".asJson,
        "features" -> List(
          "structured-editing",
          "parsing",
          "xforms",
          "repository",
          "hash-consing"
        ).asJson
      ))

    // =========================================================================
    // Repository
    // =========================================================================

    case GET -> Root / "repo" / "lc" / "info" =>
      val repo = state.lcRepo
      Ok(RepoInfo(
        branches = repo.listBranches.toList,
        currentBranch = repo.getCurrentBranch,
        head = repo.head.map(_.value),
        termCount = repo.listHashes.size,
        names = repo.listNames.map(_.toString).toList
      ).asJson)

    case GET -> Root / "repo" / "lc" / "terms" =>
      val repo = state.lcRepo
      val terms = repo.listHashes.toList.flatMap { hash =>
        repo.get(hash).map(t => Json.obj(
          "hash" -> hash.value.asJson,
          "term" -> t.asJson
        ))
      }
      Ok(terms.asJson)

    case GET -> Root / "repo" / "lc" / "term" / hashStr =>
      val hash = Hash(hashStr)
      state.lcRepo.get(hash) match
        case Some(term) => Ok(TermResponse(hashStr, term, term.isDone).asJson)
        case None => NotFound(Json.obj("error" -> s"Term not found: $hashStr".asJson))

    case GET -> Root / "repo" / "lc" / "name" / name =>
      val n = Name(name)
      state.lcRepo.getByName(n) match
        case Some(term) =>
          val hash = state.lcRepo.getHash(n).get
          Ok(TermResponse(hash.value, term, term.isDone).asJson)
        case None => NotFound(Json.obj("error" -> s"Name not found: $name".asJson))

    case req @ POST -> Root / "repo" / "lc" / "store" =>
      for
        term <- req.as[Term[LC]]
        hash = state.lcRepo.store(term)
        resp <- Ok(Json.obj("hash" -> hash.value.asJson))
      yield resp

    case req @ POST -> Root / "repo" / "lc" / "store" / name =>
      for
        term <- req.as[Term[LC]]
        hash = state.lcRepo.store(term, Set(Name(name)))
        resp <- Ok(Json.obj("hash" -> hash.value.asJson, "name" -> name.asJson))
      yield resp

    // =========================================================================
    // Parsing
    // =========================================================================

    case req @ POST -> Root / "parse" / "expr" =>
      for
        pr <- req.as[ParseRequest]
        tokens = Lexer.tokenize(pr.input)
        result = ExprParser.expr().parse(tokens)
        resp <- Ok(Json.obj(
          "term" -> result.term.asJson,
          "isComplete" -> result.term.isDone.asJson,
          "remaining" -> result.remaining.remaining.map(_.render).mkString.asJson
        ))
      yield resp

    case req @ POST -> Root / "render" / "expr" =>
      for
        term <- req.as[Term[Expr]]
        rendered = Renderer.render(ExprParser.expr(), term)
        resp <- Ok(Json.obj("rendered" -> rendered.asJson))
      yield resp

    // =========================================================================
    // Evaluation
    // =========================================================================

    case req @ POST -> Root / "eval" / "expr" =>
      for
        er <- req.as[EvalRequest]
        tokens = Lexer.tokenize(er.input)
        result = ExprParser.expr().parse(tokens)
        resp <- result.term match
          case Term.Done(expr) =>
            val value = ExprEvaluator.eval(expr, er.env)
            Ok(Json.obj("value" -> value.asJson))
          case Term.Hole(l) =>
            BadRequest(Json.obj("error" -> s"Incomplete expression: ${l.getOrElse("?")}".asJson))
      yield resp

    case req @ POST -> Root / "eval" / "lc" =>
      for
        pr <- req.as[ParseRequest]
        result = Pipeline.parseLC(pr.input)
        resp <- result match
          case Right(term) =>
            term match
              case Term.Done(lc) =>
                try
                  val value = LCInterpreter.eval(lc)
                  Ok(Json.obj(
                    "success" -> true.asJson,
                    "result" -> LCInterpreter.render(value).asJson,
                    "input" -> pr.input.asJson
                  ))
                catch
                  case e: RuntimeException =>
                    Ok(Json.obj(
                      "success" -> false.asJson,
                      "error" -> e.getMessage.asJson
                    ))
              case Term.Hole(l) =>
                BadRequest(Json.obj("error" -> s"Incomplete: ${l.getOrElse("?")}".asJson))
          case Left(err) =>
            BadRequest(Json.obj("error" -> err.asJson))
      yield resp

    case req @ POST -> Root / "reduce" / "lc" =>
      for
        pr <- req.as[ParseRequest]
        result = Pipeline.parseLC(pr.input)
        resp <- result match
          case Right(term) =>
            term match
              case Term.Done(lc) =>
                val reduced = LCInterpreter.reduce(lc)
                Ok(Json.obj(
                  "success" -> true.asJson,
                  "input" -> pr.input.asJson,
                  "reduced" -> Pipeline.renderLC(Term.Done(reduced)).asJson
                ))
              case Term.Hole(l) =>
                BadRequest(Json.obj("error" -> s"Incomplete: ${l.getOrElse("?")}".asJson))
          case Left(err) =>
            BadRequest(Json.obj("error" -> err.asJson))
      yield resp

    // =========================================================================
    // LC Parsing
    // =========================================================================

    case req @ POST -> Root / "parse" / "lc" =>
      for
        pr <- req.as[ParseRequest]
        result = Pipeline.parseLC(pr.input)
        resp <- result match
          case Right(term) =>
            Ok(Json.obj(
              "success" -> true.asJson,
              "term" -> term.asJson,
              "isComplete" -> term.isDone.asJson,
              "rendered" -> Pipeline.renderLC(term).asJson
            ))
          case Left(err) =>
            Ok(Json.obj(
              "success" -> false.asJson,
              "error" -> err.asJson
            ))
      yield resp

    case req @ POST -> Root / "render" / "lc" =>
      for
        term <- req.as[Term[LC]]
        rendered = Pipeline.renderLC(term)
        resp <- Ok(Json.obj("rendered" -> rendered.asJson))
      yield resp

    // =========================================================================
    // Transformations
    // =========================================================================

    case GET -> Root / "xforms" =>
      Ok(XformRegistry.list.toList.asJson)

    case req @ POST -> Root / "xform" / "lc-to-ic" =>
      for
        term <- req.as[Term[LC]]
        result = LCToIC.forward(term)
        resp <- Ok(Json.obj(
          "success" -> result.isDone.asJson,
          "nodeCount" -> (result match {
            case Term.Done(net) => net.nodes.size
            case _ => 0
          }).asJson
        ))
      yield resp

    case req @ POST -> Root / "xform" / "typecheck" =>
      for
        term <- req.as[Term[LC]]
        result = TypeChecker.forward(term)
        resp <- Ok(Json.obj(
          "success" -> result.isDone.asJson,
          "result" -> (result match {
            case Term.Done(typed) => typed.getType.render.asJson
            case Term.Hole(l) => l.getOrElse("type error").asJson
          })
        ))
      yield resp

    // =========================================================================
    // Structured Editor
    // =========================================================================

    case GET -> Root / "editor" / "lc" / id =>
      val editor = state.getOrCreateLCEditor(id)
      Ok(EditorState(
        id = id,
        termType = "LC",
        isComplete = editor.current.isDone,
        canUndo = editor.undoStack.nonEmpty,
        canRedo = editor.redoStack.nonEmpty
      ).asJson)

    case GET -> Root / "editor" / "lc" / id / "term" =>
      val editor = state.getOrCreateLCEditor(id)
      Ok(editor.current.asJson)

    case req @ POST -> Root / "editor" / "lc" / id / "set" =>
      for
        term <- req.as[Term[LC]]
        _ = state.editors(id) = Editor(term)
        resp <- Ok(Json.obj("success" -> true.asJson))
      yield resp

    case req @ POST -> Root / "editor" / "lc" / id / "change" =>
      for
        cr <- req.as[ChangeRequest]
        editor = state.getOrCreateLCEditor(id)
        change = cr.changeType match
          case "insert" =>
            cr.value.flatMap(_.as[LC].toOption).map(Change.insert[LC])
          case "delete" =>
            Some(Change.delete[LC])
          case "replace" =>
            cr.value.flatMap(_.as[Term[LC]].toOption).map(Change.replace[LC])
          case _ => None
        resp <- change match
          case Some(c) =>
            val newEditor = editor.applyChange(c)
            state.editors(id) = newEditor
            Ok(Json.obj(
              "success" -> true.asJson,
              "term" -> newEditor.current.asJson
            ))
          case None =>
            BadRequest(Json.obj("error" -> "Invalid change".asJson))
      yield resp

    case POST -> Root / "editor" / "lc" / id / "undo" =>
      val editor = state.getOrCreateLCEditor(id)
      editor.undo match
        case Some(newEditor) =>
          state.editors(id) = newEditor
          Ok(Json.obj("success" -> true.asJson, "term" -> newEditor.current.asJson))
        case None =>
          Ok(Json.obj("success" -> false.asJson, "error" -> "Nothing to undo".asJson))

    case POST -> Root / "editor" / "lc" / id / "redo" =>
      val editor = state.getOrCreateLCEditor(id)
      editor.redo match
        case Some(newEditor) =>
          state.editors(id) = newEditor
          Ok(Json.obj("success" -> true.asJson, "term" -> newEditor.current.asJson))
        case None =>
          Ok(Json.obj("success" -> false.asJson, "error" -> "Nothing to redo".asJson))

    // =========================================================================
    // Expression Editor
    // =========================================================================

    case GET -> Root / "editor" / "expr" / id =>
      val editor = state.getOrCreateExprEditor(id)
      Ok(EditorState(
        id = id,
        termType = "Expr",
        isComplete = editor.current.isDone,
        canUndo = editor.undoStack.nonEmpty,
        canRedo = editor.redoStack.nonEmpty
      ).asJson)

    case GET -> Root / "editor" / "expr" / id / "term" =>
      val editor = state.getOrCreateExprEditor(id)
      Ok(editor.current.asJson)

    case req @ POST -> Root / "editor" / "expr" / id / "parse" =>
      for
        pr <- req.as[ParseRequest]
        tokens = Lexer.tokenize(pr.input)
        result = ExprParser.expr().parse(tokens)
        _ = state.editors(id) = Editor(result.term)
        resp <- Ok(Json.obj(
          "success" -> true.asJson,
          "term" -> result.term.asJson,
          "isComplete" -> result.term.isDone.asJson
        ))
      yield resp

    // =========================================================================
    // Repository History & Commit
    // =========================================================================

    case GET -> Root / "repo" / "lc" / "history" =>
      val repo = state.lcRepo
      val patchHistory = repo.listPatches.map { patch =>
        Json.obj(
          "id" -> patch.id.value.asJson,
          "description" -> patch.description.asJson,
          "timestamp" -> patch.timestamp.asJson,
          "dependencies" -> patch.dependencies.map(_.value).asJson
        )
      }
      Ok(Json.obj(
        "branch" -> repo.getCurrentBranch.asJson,
        "head" -> repo.head.map(_.value).asJson,
        "patches" -> patchHistory.asJson
      ))

    case GET -> Root / "repo" / "lc" / "patch" / patchId =>
      state.lcRepo.getPatch(Hash(patchId)) match
        case Some(patch) =>
          Ok(Json.obj(
            "id" -> patch.id.value.asJson,
            "description" -> patch.description.asJson,
            "timestamp" -> patch.timestamp.asJson,
            "dependencies" -> patch.dependencies.map(_.value).asJson
          ))
        case None => NotFound(Json.obj("error" -> s"Patch not found: $patchId".asJson))

    case GET -> Root / "repo" / "lc" / "branches" =>
      val repo = state.lcRepo
      val branchList = repo.listBranches.toList.map { name =>
        Json.obj(
          "name" -> name.asJson,
          "head" -> repo.getBranchHead(name).map(_.value).asJson,
          "isCurrent" -> (name == repo.getCurrentBranch).asJson
        )
      }
      Ok(branchList.asJson)

    case req @ POST -> Root / "repo" / "lc" / "commit" =>
      for
        cr <- req.as[CommitRequest]
        parsed = Pipeline.parseLC(cr.code)
        resp <- parsed match
          case Right(term) =>
            val repo = state.lcRepo
            val currentTerm = repo.currentTerm.getOrElse(Term.hole[LC])
            val change = Change.replace[LC](term)
            val patch = Patch.create(cr.message, change, currentTerm)
            repo.applyPatch(patch, currentTerm)
            val hash = repo.store(term, cr.name.map(n => Set(Name(n))).getOrElse(Set.empty))
            Ok(Json.obj(
              "success" -> true.asJson,
              "hash" -> hash.value.asJson,
              "patchId" -> patch.id.value.asJson,
              "message" -> cr.message.asJson
            ))
          case Left(err) =>
            BadRequest(Json.obj("success" -> false.asJson, "error" -> err.asJson))
      yield resp

    case req @ POST -> Root / "repo" / "lc" / "branch" / "create" =>
      for
        br <- req.as[BranchRequest]
        _ = state.lcRepo.createBranch(br.name)
        resp <- Ok(Json.obj("success" -> true.asJson, "branch" -> br.name.asJson))
      yield resp

    case POST -> Root / "repo" / "lc" / "branch" / "switch" / branchName =>
      if state.lcRepo.switchBranch(branchName) then
        Ok(Json.obj("success" -> true.asJson, "branch" -> branchName.asJson))
      else
        NotFound(Json.obj("success" -> false.asJson, "error" -> s"Branch not found: $branchName".asJson))

    case POST -> Root / "repo" / "lc" / "merge" / sourceBranch =>
      state.lcRepo.merge(sourceBranch) match
        case MergeResult.Success(term) =>
          Ok(Json.obj("success" -> true.asJson, "result" -> "merged".asJson))
        case MergeResult.AlreadyUpToDate =>
          Ok(Json.obj("success" -> true.asJson, "result" -> "already-up-to-date".asJson))
        case MergeResult.Conflict(_) =>
          Ok(Json.obj("success" -> false.asJson, "result" -> "conflict".asJson))
        case MergeResult.BranchNotFound =>
          NotFound(Json.obj("success" -> false.asJson, "error" -> s"Branch not found: $sourceBranch".asJson))

  }
object PhiServer extends IOApp.Simple:
  val state = new ServerState

  def run: IO[Unit] =
    val app = PhiRoutes.routes(state).orNotFound

    EmberServerBuilder
      .default[IO]
      .withHost(host"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(app)
      .build
      .use { server =>
        IO.println(s"🔮 Phi server started at http://localhost:8080") *>
        IO.never
      }
