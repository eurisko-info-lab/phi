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

  def getOrCreateLCEditor(id: String): Editor[LC] =
    editors.getOrElseUpdate(id, Editor.empty[LC]).asInstanceOf[Editor[LC]]

  def getOrCreateExprEditor(id: String): Editor[Expr] =
    editors.getOrElseUpdate(id, Editor.empty[Expr]).asInstanceOf[Editor[Expr]]

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
    // Static HTML Interface
    // =========================================================================

    case GET -> Root =>
      IO.pure(Response[IO](Status.Ok)
        .withBodyStream(fs2.Stream.emits(PhiHtml.indexPage.getBytes("UTF-8")))
        .withContentType(`Content-Type`(MediaType.text.html, Charset.`UTF-8`)))

    case GET -> Root / "editor.html" =>
      IO.pure(Response[IO](Status.Ok)
        .withBodyStream(fs2.Stream.emits(PhiHtml.editorPage.getBytes("UTF-8")))
        .withContentType(`Content-Type`(MediaType.text.html, Charset.`UTF-8`)))
  }

// =============================================================================
// HTML Templates
// =============================================================================

object PhiHtml:
  val indexPage: String = """<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Phi Language Server</title>
  <style>
    body { font-family: system-ui, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
    h1 { color: #333; }
    .section { margin: 20px 0; padding: 15px; border: 1px solid #ddd; border-radius: 8px; }
    code { background: #f5f5f5; padding: 2px 6px; border-radius: 4px; }
    a { color: #0066cc; }
    pre { background: #f5f5f5; padding: 15px; border-radius: 8px; overflow-x: auto; }
  </style>
</head>
<body>
  <h1>🔮 Phi Language Server</h1>
  
  <div class="section">
    <h2>Features</h2>
    <ul>
      <li><strong>Structured Editing</strong> - Edit terms with holes, undo/redo</li>
      <li><strong>Parsing</strong> - Parse expressions to terms with holes for incomplete input</li>
      <li><strong>Transformations</strong> - LC ↔ IC, type checking, pattern compilation</li>
      <li><strong>Repository</strong> - Content-addressed storage with branches and patches</li>
      <li><strong>Hash-Consing</strong> - Canonical subterm sharing</li>
    </ul>
  </div>

  <div class="section">
    <h2>API Endpoints</h2>
    <h3>Health & Info</h3>
    <pre>GET /health
GET /info</pre>

    <h3>Repository</h3>
    <pre>GET /repo/lc/info
GET /repo/lc/terms
GET /repo/lc/term/:hash
GET /repo/lc/name/:name
POST /repo/lc/store
POST /repo/lc/store/:name</pre>

    <h3>Parsing & Rendering</h3>
    <pre>POST /parse/expr    { "input": "1 + 2 * 3" }
POST /render/expr   { "type": "done", "value": ... }
POST /eval/expr     { "input": "x + 1", "env": {"x": 5} }</pre>

    <h3>Transformations</h3>
    <pre>GET /xforms
POST /xform/lc-to-ic
POST /xform/typecheck</pre>

    <h3>Structured Editor</h3>
    <pre>GET /editor/lc/:id
GET /editor/lc/:id/term
POST /editor/lc/:id/set
POST /editor/lc/:id/change
POST /editor/lc/:id/undo
POST /editor/lc/:id/redo</pre>
  </div>

  <div class="section">
    <h2>Try It</h2>
    <p><a href="/editor.html">Open Structured Editor</a></p>
  </div>
</body>
</html>
"""

  val editorPage: String = """<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Phi Structured Editor</title>
  <style>
    * { box-sizing: border-box; }
    body { font-family: system-ui, -apple-system, sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; background: #1a1a2e; color: #eee; }
    h1 { color: #fff; margin-bottom: 5px; }
    h1 a { font-size: 14px; font-weight: normal; margin-left: 10px; color: #88f; }
    h3 { margin: 15px 0 10px 0; color: #aaa; border-bottom: 1px solid #333; padding-bottom: 5px; }
    .editor { display: flex; gap: 20px; flex-wrap: wrap; }
    .panel { flex: 1; min-width: 450px; background: #16213e; border: 1px solid #0f3460; border-radius: 12px; padding: 20px; }
    textarea { width: 100%; height: 60px; font-family: 'JetBrains Mono', 'SF Mono', Monaco, Consolas, monospace; font-size: 16px; padding: 12px; border: 2px solid #0f3460; border-radius: 6px; background: #0f0f23; color: #0f0; resize: vertical; }
    textarea:focus { outline: none; border-color: #e94560; }
    .buttons { margin: 12px 0; display: flex; gap: 8px; flex-wrap: wrap; }
    button { background: linear-gradient(to bottom, #e94560, #c73e54); color: white; border: none; padding: 10px 16px; border-radius: 6px; cursor: pointer; font-size: 13px; font-weight: 600; transition: all 0.2s; }
    button:hover { background: linear-gradient(to bottom, #ff5a7a, #e94560); transform: translateY(-1px); }
    button.secondary { background: linear-gradient(to bottom, #0f3460, #0a2647); }
    button.secondary:hover { background: linear-gradient(to bottom, #1a4a80, #0f3460); }
    .output { background: #0f0f23; padding: 15px; border-radius: 8px; font-family: 'JetBrains Mono', monospace; font-size: 13px; min-height: 60px; border: 1px solid #0f3460; overflow-x: auto; }
    
    /* Hazel-like AST visualization */
    .ast { font-family: 'JetBrains Mono', monospace; }
    .ast-node { display: inline-block; padding: 4px 8px; margin: 2px; border-radius: 4px; }
    .ast-var { background: #264653; color: #2a9d8f; }
    .ast-lam { background: #2d3a4f; border-left: 3px solid #e9c46a; }
    .ast-app { background: #2d3a4f; border-left: 3px solid #f4a261; }
    .ast-let { background: #2d3a4f; border-left: 3px solid #e76f51; }
    .ast-lit { background: #1d3557; color: #a8dadc; }
    .ast-prim { background: #3d2944; color: #f4a8ba; }
    .ast-hole { background: #5c2a2a; color: #ffcc00; border: 2px dashed #ffcc00; animation: pulse 1s infinite; }
    .ast-keyword { color: #e94560; font-weight: bold; }
    .ast-param { color: #e9c46a; font-style: italic; }
    .ast-op { color: #f4a261; font-weight: bold; }
    @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.6; } }
    
    /* Structured editor display */
    .struct-editor { background: #0f0f23; border: 2px solid #0f3460; border-radius: 8px; padding: 20px; margin: 10px 0; min-height: 100px; }
    .struct-editor:focus-within { border-color: #e94560; }
    .struct-expr { display: inline-flex; align-items: center; gap: 4px; }
    .struct-binop { display: inline-flex; align-items: center; padding: 4px 8px; background: #1a1a3e; border-radius: 6px; margin: 2px; }
    .struct-atom { padding: 4px 10px; border-radius: 4px; cursor: pointer; }
    .struct-atom:hover { filter: brightness(1.2); }
    .struct-num { background: #1d3557; color: #a8dadc; }
    .struct-var { background: #264653; color: #2a9d8f; }
    .struct-op { padding: 4px 8px; color: #f4a261; font-weight: bold; }
    .struct-paren { color: #666; font-size: 1.2em; }
    .struct-hole { background: #5c2a2a; color: #ffcc00; border: 2px dashed #ffcc00; min-width: 30px; text-align: center; }
    .struct-let { display: block; padding: 8px; background: #1a2a3a; border-radius: 6px; border-left: 3px solid #e76f51; margin: 4px 0; }
    .struct-if { display: block; padding: 8px; background: #1a2a3a; border-radius: 6px; border-left: 3px solid #9b59b6; margin: 4px 0; }
    
    .type-info { color: #888; font-size: 12px; margin-top: 8px; }
    .success { color: #2a9d8f; font-weight: 500; }
    .error { color: #e94560; font-weight: 500; }
    
    /* Mini toolbar */
    .mini-toolbar { display: flex; gap: 4px; margin: 8px 0; flex-wrap: wrap; }
    .mini-btn { background: #1a1a3e; color: #aaa; border: 1px solid #333; padding: 4px 10px; border-radius: 4px; cursor: pointer; font-size: 12px; }
    .mini-btn:hover { background: #2a2a4e; color: #fff; }
  </style>
</head>
<body>
  <h1>🔮 Phi Structured Editor <a href="/">← API docs</a></h1>
  
  <div class="editor">
    <div class="panel">
      <h3>📝 Expression Editor</h3>
      <textarea id="input" oninput="liveParseExpr()">let x = 5 in x * 2 + 1</textarea>
      <div class="mini-toolbar">
        <button class="mini-btn" onclick="insertAtCursor('let ? = ? in ?')">let</button>
        <button class="mini-btn" onclick="insertAtCursor('if ? then ? else ?')">if</button>
        <button class="mini-btn" onclick="insertAtCursor('(?)')">( )</button>
        <button class="mini-btn" onclick="insertAtCursor(' + ')">+</button>
        <button class="mini-btn" onclick="insertAtCursor(' * ')">*</button>
        <button class="mini-btn" onclick="insertAtCursor('?')">hole</button>
      </div>
      
      <h3>🌳 Structured View</h3>
      <div id="structView" class="struct-editor">Type an expression above...</div>
      
      <div class="buttons">
        <button onclick="evalExpr()">▶ Evaluate</button>
        <button class="secondary" onclick="roundTrip()">↔ Round-Trip</button>
      </div>
      
      <h3>📊 Result</h3>
      <div id="result" class="output">Click "Evaluate" to compute</div>
    </div>
    
    <div class="panel">
      <h3>λ Lambda Calculus</h3>
      <textarea id="lcInput" oninput="liveParseLambda()">fn x => fn y => x + y</textarea>
      <div class="mini-toolbar">
        <button class="mini-btn" onclick="insertLCAtCursor('fn ? => ?')">λ</button>
        <button class="mini-btn" onclick="insertLCAtCursor('let ? = ? in ?')">let</button>
        <button class="mini-btn" onclick="insertLCAtCursor('(? ?)')">app</button>
        <button class="mini-btn" onclick="insertLCAtCursor('?')">hole</button>
      </div>
      
      <h3>🌳 AST View</h3>
      <div id="lcTerm" class="struct-editor">Type a lambda expression above...</div>
      
      <div class="buttons">
        <button onclick="typeCheck()">🔍 Type Check</button>
        <button class="secondary" onclick="compileToIC()">⚡ Compile to IC</button>
      </div>
      
      <h3>📊 Analysis</h3>
      <div id="lcResult" class="output">Results will appear here</div>
    </div>
  </div>

  <script>
    const API = '';
    
    // Live parsing as you type
    let parseTimeout = null;
    function liveParseExpr() {
      clearTimeout(parseTimeout);
      parseTimeout = setTimeout(parseExpr, 150);
    }
    function liveParseLambda() {
      clearTimeout(parseTimeout);
      parseTimeout = setTimeout(parseLambda, 150);
    }
    
    // Insert template at cursor
    function insertAtCursor(text) {
      const ta = document.getElementById('input');
      const start = ta.selectionStart, end = ta.selectionEnd;
      ta.value = ta.value.substring(0, start) + text + ta.value.substring(end);
      ta.selectionStart = ta.selectionEnd = start + text.indexOf('?');
      if (text.indexOf('?') < 0) ta.selectionStart = ta.selectionEnd = start + text.length;
      ta.focus();
      liveParseExpr();
    }
    function insertLCAtCursor(text) {
      const ta = document.getElementById('lcInput');
      const start = ta.selectionStart, end = ta.selectionEnd;
      ta.value = ta.value.substring(0, start) + text + ta.value.substring(end);
      ta.selectionStart = ta.selectionEnd = start + text.indexOf('?');
      if (text.indexOf('?') < 0) ta.selectionStart = ta.selectionEnd = start + text.length;
      ta.focus();
      liveParseLambda();
    }
    
    async function parseExpr() {
      const input = document.getElementById('input').value;
      if (!input.trim()) {
        document.getElementById('structView').innerHTML = '<span class="ast-hole">?</span>';
        return;
      }
      try {
        const resp = await fetch(API + '/parse/expr', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({input})
        });
        const data = await resp.json();
        document.getElementById('structView').innerHTML = renderExprAST(data.term);
        if (!data.isComplete) {
          document.getElementById('result').innerHTML = '<span class="type-info">Expression has holes - fill them in</span>';
        }
      } catch (e) {
        document.getElementById('structView').innerHTML = '<span class="error">Parse error</span>';
      }
    }
    
    function renderExprAST(term) {
      if (term.type === 'hole') {
        return '<span class="struct-atom struct-hole">?' + (term.label || '') + '</span>';
      }
      const v = term.value;
      if (!v) return '<span class="struct-atom struct-hole">?</span>';
      
      switch (v.type) {
        case 'num':
          return '<span class="struct-atom struct-num">' + v.value + '</span>';
        case 'var':
          return '<span class="struct-atom struct-var">' + v.name + '</span>';
        case 'binop':
          return '<span class="struct-binop">' + 
            renderExprAST({type:'done', value: v.left}) + 
            '<span class="struct-op">' + v.op + '</span>' +
            renderExprAST({type:'done', value: v.right}) + '</span>';
        case 'paren':
          return '<span class="struct-paren">(</span>' + 
            renderExprAST({type:'done', value: v.inner}) + 
            '<span class="struct-paren">)</span>';
        case 'let':
          return '<div class="struct-let"><span class="ast-keyword">let</span> ' +
            '<span class="ast-param">' + v.name + '</span> = ' +
            renderExprAST({type:'done', value: v.value}) +
            ' <span class="ast-keyword">in</span><br/>' +
            renderExprAST({type:'done', value: v.body}) + '</div>';
        case 'if':
          return '<div class="struct-if"><span class="ast-keyword">if</span> ' +
            renderExprAST({type:'done', value: v.cond}) +
            ' <span class="ast-keyword">then</span> ' +
            renderExprAST({type:'done', value: v.then}) +
            ' <span class="ast-keyword">else</span> ' +
            renderExprAST({type:'done', value: v.else}) + '</div>';
        case 'call':
          return '<span class="struct-binop"><span class="ast-var">' + v.func + '</span>(' +
            v.args.map(a => renderExprAST({type:'done', value: a})).join(', ') + ')</span>';
        case 'unary':
          return '<span class="struct-binop"><span class="struct-op">' + v.op + '</span>' +
            renderExprAST({type:'done', value: v.operand}) + '</span>';
        default:
          return '<span class="struct-atom struct-hole">?</span>';
      }
    }
    
    async function evalExpr() {
      const input = document.getElementById('input').value;
      try {
        const resp = await fetch(API + '/eval/expr', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({input, env: {}})  
        });
        const data = await resp.json();
        if (data.error) {
          document.getElementById('result').innerHTML = '<span class="error">' + esc(data.error) + '</span>';
        } else {
          document.getElementById('result').innerHTML = '<span class="success">= ' + data.value + '</span>';
        }
      } catch (e) {
        document.getElementById('result').innerHTML = '<span class="error">Error: ' + e.message + '</span>';
      }
    }
    
    async function roundTrip() {
      const input = document.getElementById('input').value;
      try {
        const parseResp = await fetch(API + '/parse/expr', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({input})
        });
        const parseData = await parseResp.json();
        const renderResp = await fetch(API + '/render/expr', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify(parseData.term)
        });
        const renderData = await renderResp.json();
        const match = input.replace(/\s+/g, ' ').trim() === renderData.rendered.replace(/\s+/g, ' ').trim();
        document.getElementById('result').innerHTML = 
          '<strong>Input:</strong> ' + esc(input) + '<br>' +
          '<strong>Rendered:</strong> ' + esc(renderData.rendered) + '<br>' +
          '<strong>Match:</strong> ' + (match ? '<span class="success">✓ Yes</span>' : '<span class="error">✗ No</span>');
      } catch (e) {
        document.getElementById('result').innerHTML = '<span class="error">Error: ' + e.message + '</span>';
      }
    }
    
    async function parseLambda() {
      const input = document.getElementById('lcInput').value;
      if (!input.trim()) {
        document.getElementById('lcTerm').innerHTML = '<span class="ast-hole">?</span>';
        return;
      }
      try {
        const resp = await fetch(API + '/parse/lc', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({input})
        });
        const data = await resp.json();
        if (data.success) {
          document.getElementById('lcTerm').innerHTML = renderLCAST(data.term) + 
            '<div class="type-info">Rendered: ' + esc(data.rendered) + '</div>';
        } else {
          document.getElementById('lcTerm').innerHTML = '<span class="error">' + esc(data.error) + '</span>';
        }
      } catch (e) {
        document.getElementById('lcTerm').innerHTML = '<span class="error">Error: ' + e.message + '</span>';
      }
    }
    
    function renderLCAST(term) {
      if (term.type === 'hole') {
        return '<span class="ast-node ast-hole">?' + (term.label || '') + '</span>';
      }
      const v = term.value;
      if (!v) return '<span class="ast-node ast-hole">?</span>';
      
      switch (v.type) {
        case 'var':
          return '<span class="ast-node ast-var">' + v.name + '</span>';
        case 'lam':
          return '<span class="ast-node ast-lam"><span class="ast-keyword">λ</span>' +
            '<span class="ast-param">' + v.param + '</span>.' +
            renderLCAST({type:'done', value: v.body}) + '</span>';
        case 'app':
          return '<span class="ast-node ast-app">(' +
            renderLCAST({type:'done', value: v.func}) + ' ' +
            renderLCAST({type:'done', value: v.arg}) + ')</span>';
        case 'let':
          return '<span class="ast-node ast-let"><span class="ast-keyword">let</span> ' +
            '<span class="ast-param">' + v.name + '</span> = ' +
            renderLCAST({type:'done', value: v.value}) +
            ' <span class="ast-keyword">in</span> ' +
            renderLCAST({type:'done', value: v.body}) + '</span>';
        case 'lit':
          return '<span class="ast-node ast-lit">' + v.value + '</span>';
        case 'prim':
          // Render binary ops in infix notation
          if (v.args && v.args.length === 2 && ['+','-','*','/'].includes(v.op)) {
            return '<span class="ast-node ast-prim">' +
              renderLCAST({type:'done', value: v.args[0]}) +
              ' <span class="ast-op">' + v.op + '</span> ' +
              renderLCAST({type:'done', value: v.args[1]}) + '</span>';
          }
          return '<span class="ast-node ast-prim">' + v.op + '(' +
            (v.args || []).map(a => renderLCAST({type:'done', value: a})).join(', ') + ')</span>';
        default:
          return '<span class="ast-node ast-hole">?</span>';
      }
    }
    
    async function typeCheck() {
      const input = document.getElementById('lcInput').value;
      try {
        const parseResp = await fetch(API + '/parse/lc', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({input})
        });
        const parseData = await parseResp.json();
        if (!parseData.success) {
          document.getElementById('lcResult').innerHTML = '<span class="error">Parse error: ' + esc(parseData.error) + '</span>';
          return;
        }
        const resp = await fetch(API + '/xform/typecheck', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify(parseData.term)
        });
        const data = await resp.json();
        document.getElementById('lcResult').innerHTML = 
          '<strong>Type:</strong> ' + esc(data.result) +
          (data.success ? ' <span class="success">✓</span>' : ' <span class="error">✗</span>');
      } catch (e) {
        document.getElementById('lcResult').innerHTML = '<span class="error">Error: ' + e.message + '</span>';
      }
    }
    
    async function compileToIC() {
      const input = document.getElementById('lcInput').value;
      try {
        const parseResp = await fetch(API + '/parse/lc', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({input})
        });
        const parseData = await parseResp.json();
        if (!parseData.success) {
          document.getElementById('lcResult').innerHTML = '<span class="error">Parse error: ' + esc(parseData.error) + '</span>';
          return;
        }
        const resp = await fetch(API + '/xform/lc-to-ic', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify(parseData.term)
        });
        const data = await resp.json();
        document.getElementById('lcResult').innerHTML = 
          '<strong>IC Compilation:</strong> ' + data.nodeCount + ' nodes' +
          (data.success ? ' <span class="success">✓</span>' : ' <span class="error">✗</span>');
      } catch (e) {
        document.getElementById('lcResult').innerHTML = '<span class="error">Error: ' + e.message + '</span>';
      }
    }
    
    function esc(str) {
      if (!str) return '';
      return String(str).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    }
    
    // Initial parse on load
    window.onload = () => { parseExpr(); parseLambda(); };
  </script>
</body>
</html>
"""

// =============================================================================
// Server Entry Point
// =============================================================================

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
