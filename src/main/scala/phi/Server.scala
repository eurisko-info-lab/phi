package phi

import cats.effect.*
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.*
import org.http4s.implicits.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.*
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
      Ok(PhiHtml.indexPage).map(_.withContentType(`Content-Type`(MediaType.text.html)))

    case GET -> Root / "editor.html" =>
      Ok(PhiHtml.editorPage).map(_.withContentType(`Content-Type`(MediaType.text.html)))
  }

// =============================================================================
// HTML Templates
// =============================================================================

object PhiHtml:
  val indexPage: String = """
<!DOCTYPE html>
<html>
<head>
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

  val editorPage: String = """
<!DOCTYPE html>
<html>
<head>
  <title>Phi Structured Editor</title>
  <style>
    body { font-family: system-ui, sans-serif; max-width: 1000px; margin: 0 auto; padding: 20px; }
    h1 { color: #333; }
    .editor { display: flex; gap: 20px; }
    .panel { flex: 1; border: 1px solid #ddd; border-radius: 8px; padding: 15px; }
    textarea { width: 100%; height: 100px; font-family: monospace; padding: 10px; border: 1px solid #ccc; border-radius: 4px; }
    button { background: #0066cc; color: white; border: none; padding: 8px 16px; border-radius: 4px; cursor: pointer; margin: 5px 5px 5px 0; }
    button:hover { background: #0055aa; }
    button:disabled { background: #ccc; }
    .output { background: #f5f5f5; padding: 15px; border-radius: 8px; font-family: monospace; white-space: pre-wrap; min-height: 100px; }
    .term { background: #e8f4e8; padding: 10px; border-radius: 4px; margin: 10px 0; }
    .hole { background: #fff3cd; padding: 2px 6px; border-radius: 4px; border: 1px dashed #ffc107; }
    .error { color: #dc3545; }
    .success { color: #28a745; }
  </style>
</head>
<body>
  <h1>🔮 Phi Structured Editor</h1>
  
  <div class="editor">
    <div class="panel">
      <h3>Expression Input</h3>
      <textarea id="input" placeholder="Enter expression, e.g.: 1 + 2 * 3">let x = 5 in x * 2 + 1</textarea>
      <div>
        <button onclick="parseExpr()">Parse</button>
        <button onclick="evalExpr()">Evaluate</button>
        <button onclick="roundTrip()">Round-Trip</button>
      </div>
      
      <h3>Parsed Term</h3>
      <div id="term" class="output">Click "Parse" to see the term</div>
      
      <h3>Result</h3>
      <div id="result" class="output">Click "Evaluate" to see the result</div>
    </div>
    
    <div class="panel">
      <h3>Lambda Calculus</h3>
      <textarea id="lcInput" placeholder="e.g.: fn x => x + 1">fn x => fn y => x + y</textarea>
      <div>
        <button onclick="parseLambda()">Parse LC</button>
        <button onclick="typeCheck()">Type Check</button>
        <button onclick="compileToIC()">Compile to IC</button>
      </div>
      
      <h3>LC Term</h3>
      <div id="lcTerm" class="output">Click "Parse LC" to see the term</div>
      
      <h3>Type / IC Info</h3>
      <div id="lcResult" class="output">Results will appear here</div>
    </div>
  </div>

  <script>
    const API = '';
    
    async function parseExpr() {
      const input = document.getElementById('input').value;
      const resp = await fetch(API + '/parse/expr', {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({input})
      });
      const data = await resp.json();
      document.getElementById('term').innerHTML = formatTerm(data.term);
      if (!data.isComplete) {
        document.getElementById('result').innerHTML = '<span class="error">Incomplete expression</span>';
      }
    }
    
    async function evalExpr() {
      const input = document.getElementById('input').value;
      const resp = await fetch(API + '/eval/expr', {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({input, env: {}})
      });
      const data = await resp.json();
      if (data.error) {
        document.getElementById('result').innerHTML = '<span class="error">' + data.error + '</span>';
      } else {
        document.getElementById('result').innerHTML = '<span class="success">Result: ' + data.value + '</span>';
      }
    }
    
    async function roundTrip() {
      const input = document.getElementById('input').value;
      // Parse
      const parseResp = await fetch(API + '/parse/expr', {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({input})
      });
      const parseData = await parseResp.json();
      
      // Render
      const renderResp = await fetch(API + '/render/expr', {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify(parseData.term)
      });
      const renderData = await renderResp.json();
      
      document.getElementById('result').innerHTML = 
        '<strong>Original:</strong> ' + input + '\\n' +
        '<strong>Round-trip:</strong> ' + renderData.rendered + '\\n' +
        '<strong>Match:</strong> ' + (input.replace(/\\s+/g, ' ').trim() === renderData.rendered.replace(/\\s+/g, ' ').trim() ? '✅ Yes' : '❌ No');
    }
    
    async function typeCheck() {
      const input = document.getElementById('lcInput').value;
      // For now, show a placeholder - would need LC parser
      document.getElementById('lcResult').innerHTML = 'Type checking: ' + input;
    }
    
    async function compileToIC() {
      document.getElementById('lcResult').innerHTML = 'IC compilation not yet connected to UI';
    }
    
    function parseLambda() {
      document.getElementById('lcTerm').innerHTML = 'LC parsing not yet connected to UI';
    }
    
    function formatTerm(term) {
      if (term.type === 'done') {
        return '<div class="term">' + JSON.stringify(term.value, null, 2) + '</div>';
      } else {
        return '<span class="hole">?' + (term.label || '') + '</span>';
      }
    }
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
