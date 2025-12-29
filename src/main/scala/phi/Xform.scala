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
// Lambda Calculus (LC) AST
// =============================================================================

/**
 * Untyped Lambda Calculus terms.
 */
enum LC:
  case Var(name: String)
  case Lam(param: String, body: LC)
  case App(func: LC, arg: LC)
  case Let(name: String, value: LC, body: LC)
  case Lit(value: Int)
  case Prim(op: String, args: List[LC])

object LC:
  /** Free variables in a term */
  def freeVars(term: LC): Set[String] = term match
    case Var(n)        => Set(n)
    case Lam(p, b)     => freeVars(b) - p
    case App(f, a)     => freeVars(f) ++ freeVars(a)
    case Let(n, v, b)  => freeVars(v) ++ (freeVars(b) - n)
    case Lit(_)        => Set.empty
    case Prim(_, args) => args.flatMap(freeVars).toSet

  /** Substitute a variable with a term */
  def subst(term: LC, x: String, s: LC): LC = term match
    case Var(n) => if n == x then s else term
    case Lam(p, b) =>
      if p == x then term
      else if freeVars(s).contains(p) then
        val fresh = freshVar(p, freeVars(s) ++ freeVars(b))
        Lam(fresh, subst(subst(b, p, Var(fresh)), x, s))
      else Lam(p, subst(b, x, s))
    case App(f, a) => App(subst(f, x, s), subst(a, x, s))
    case Let(n, v, b) =>
      if n == x then Let(n, subst(v, x, s), b)
      else Let(n, subst(v, x, s), subst(b, x, s))
    case Lit(_) => term
    case Prim(op, args) => Prim(op, args.map(subst(_, x, s)))

  /** Generate a fresh variable name */
  def freshVar(base: String, avoid: Set[String]): String =
    var candidate = base + "'"
    while avoid.contains(candidate) do candidate = candidate + "'"
    candidate

// =============================================================================
// LC Interpreter - Beta Reduction
// =============================================================================

/**
 * Lambda Calculus interpreter with call-by-value semantics.
 */
object LCInterpreter:
  /** Result of evaluation */
  enum Value:
    case VInt(n: Int)
    case VClosure(param: String, body: LC, env: Env)
    case VPrim(op: String, args: List[Value])
  
  // Use lazy thunk for recursive bindings
  class Thunk(compute: => Value):
    lazy val value: Value = compute
  
  type Env = Map[String, Thunk]
  
  /** Evaluate LC term to a value */
  def eval(term: LC, env: Env = Map.empty): Value = term match
    case LC.Lit(n) => Value.VInt(n)
    
    case LC.Var(name) => 
      env.get(name) match
        case Some(thunk) => thunk.value
        case None => throw RuntimeException(s"Unbound variable: $name")
    
    case LC.Lam(param, body) => 
      Value.VClosure(param, body, env)
    
    case LC.App(func, arg) =>
      val fv = eval(func, env)
      val av = eval(arg, env)
      fv match
        case Value.VClosure(param, body, closureEnv) =>
          eval(body, closureEnv + (param -> Thunk(av)))
        case _ => throw RuntimeException(s"Cannot apply non-function: $fv")
    
    case LC.Let(name, value, body) =>
      // Support recursive let with lazy thunk
      lazy val thunk: Thunk = new Thunk(eval(value, env + (name -> thunk)))
      eval(body, env + (name -> thunk))
    
    case LC.Prim("if", List(cond, thenBr, elseBr)) =>
      val cv = eval(cond, env)
      cv match
        case Value.VInt(n) if n != 0 => eval(thenBr, env)
        case Value.VInt(_) => eval(elseBr, env)
        case _ => throw RuntimeException(s"if condition must be int, got $cv")
    
    case LC.Prim(op, args) =>
      val vals = args.map(eval(_, env))
      evalPrim(op, vals)
  
  /** Evaluate primitive operation */
  def evalPrim(op: String, args: List[Value]): Value =
    def toInt(v: Value): Int = v match
      case Value.VInt(n) => n
      case _ => throw RuntimeException(s"Expected int, got $v")
    
    (op, args.map(toInt)) match
      case ("+", List(a, b)) => Value.VInt(a + b)
      case ("-", List(a, b)) => Value.VInt(a - b)
      case ("*", List(a, b)) => Value.VInt(a * b)
      case ("/", List(a, b)) => Value.VInt(a / b)
      case ("%", List(a, b)) => Value.VInt(a % b)
      case ("=", List(a, b)) => Value.VInt(if a == b then 1 else 0)
      case ("!=", List(a, b)) => Value.VInt(if a != b then 1 else 0)
      case ("<", List(a, b)) => Value.VInt(if a < b then 1 else 0)
      case (">", List(a, b)) => Value.VInt(if a > b then 1 else 0)
      case ("<=", List(a, b)) => Value.VInt(if a <= b then 1 else 0)
      case (">=", List(a, b)) => Value.VInt(if a >= b then 1 else 0)
      case ("|", List(a, b)) => Value.VInt(if a != 0 || b != 0 then 1 else 0)
      case ("&", List(a, b)) => Value.VInt(if a != 0 && b != 0 then 1 else 0)
      case _ => throw RuntimeException(s"Unknown primitive: $op with ${args.size} args")
  
  /** Render value to string */
  def render(v: Value): String = v match
    case Value.VInt(n) => n.toString
    case Value.VClosure(p, _, _) => s"<closure λ$p.…>"
    case Value.VPrim(op, args) => s"<prim $op>"
  
  /** Beta-reduce LC term (normalize) with step limit */
  def reduce(term: LC, maxSteps: Int = 1000): LC =
    var current = term
    var steps = 0
    while steps < maxSteps do
      step(current) match
        case Some(next) =>
          current = next
          steps += 1
        case None =>
          return current
    current // Return current state if max steps reached
  
  /** Single beta-reduction step (leftmost-outermost) */
  def step(term: LC): Option[LC] = term match
    case LC.Var(_) => None
    case LC.Lit(_) => None
    
    case LC.App(LC.Lam(param, body), arg) =>
      // Beta reduction
      Some(LC.subst(body, param, arg))
    
    case LC.App(func, arg) =>
      // Try to reduce func first
      step(func).map(f => LC.App(f, arg))
        .orElse(step(arg).map(a => LC.App(func, a)))
    
    case LC.Lam(param, body) =>
      step(body).map(b => LC.Lam(param, b))
    
    case LC.Let(name, value, body) =>
      // Desugar let
      Some(LC.subst(body, name, value))
    
    case LC.Prim(op, args) =>
      // Try to reduce all args to literals, then compute
      val reduced = args.map(reduce(_, 100))
      val allLits = reduced.collect { case LC.Lit(n) => n }
      if allLits.size == reduced.size then
        evalPrimLC(op, allLits).map(LC.Lit.apply)
      else
        None
  
  /** Evaluate primitive on ints */
  def evalPrimLC(op: String, args: List[Int]): Option[Int] = (op, args) match
    case ("+", List(a, b)) => Some(a + b)
    case ("-", List(a, b)) => Some(a - b)
    case ("*", List(a, b)) => Some(a * b)
    case ("/", List(a, b)) if b != 0 => Some(a / b)
    case ("%", List(a, b)) if b != 0 => Some(a % b)
    case ("=", List(a, b)) => Some(if a == b then 1 else 0)
    case ("!=", List(a, b)) => Some(if a != b then 1 else 0)
    case ("<", List(a, b)) => Some(if a < b then 1 else 0)
    case (">", List(a, b)) => Some(if a > b then 1 else 0)
    case ("<=", List(a, b)) => Some(if a <= b then 1 else 0)
    case (">=", List(a, b)) => Some(if a >= b then 1 else 0)
    case ("|", List(a, b)) => Some(if a != 0 || b != 0 then 1 else 0)
    case ("&", List(a, b)) => Some(if a != 0 && b != 0 then 1 else 0)
    case _ => None

// =============================================================================
// Interaction Calculus (IC) - Net-based representation
// =============================================================================

/**
 * Interaction Calculus node types.
 */
enum ICNode:
  case Era                                    // Eraser
  case Dup(label: Int)                       // Duplicator
  case Con                                   // Constructor (λ-node)
  case Ref(name: String)                     // Reference to definition

/**
 * A port on an IC node.
 */
case class Port(nodeId: Int, portNum: Int)

/**
 * Interaction Net representation.
 */
case class ICNet(
  nodes: Map[Int, ICNode],
  links: Map[Port, Port],
  root: Port,
  nextId: Int = 0
):
  /** Add a new node */
  def addNode(node: ICNode): (ICNet, Int) =
    val id = nextId
    (copy(nodes = nodes.updated(id, node), nextId = nextId + 1), id)

  /** Link two ports */
  def link(p1: Port, p2: Port): ICNet =
    copy(links = links.updated(p1, p2).updated(p2, p1))

  /** Get linked port */
  def getLink(port: Port): Option[Port] =
    links.get(port)

  /** Get node by id */
  def getNode(id: Int): Option[ICNode] =
    nodes.get(id)

object ICNet:
  def empty: ICNet = ICNet(Map.empty, Map.empty, Port(0, 0))

  /** Create net with single root node */
  def withRoot(node: ICNode): ICNet =
    ICNet(Map(0 -> node), Map.empty, Port(0, 0), 1)

// =============================================================================
// LC ↔ IC Transformation
// =============================================================================

/**
 * Transform between Lambda Calculus and Interaction Calculus.
 */
object LCToIC extends Xform[LC, ICNet]:
  private var labelCounter = 0

  def forward(term: Term[LC]): Term[ICNet] = term match
    case Term.Done(lc) => Term.Done(compile(lc))
    case Term.Hole(l)  => Term.Hole(l)

  def backward(term: Term[ICNet]): Term[LC] = term match
    case Term.Done(net) => Term.Done(decompile(net))
    case Term.Hole(l)   => Term.Hole(l)

  /** Compile LC to IC net */
  def compile(lc: LC): ICNet =
    labelCounter = 0
    val (net, _) = compileExpr(lc, ICNet.empty, Map.empty)
    net

  private def compileExpr(lc: LC, net: ICNet, env: Map[String, Port]): (ICNet, Port) =
    lc match
      case LC.Var(name) =>
        env.get(name) match
          case Some(port) => (net, port)
          case None =>
            val (net1, id) = net.addNode(ICNode.Ref(name))
            (net1.copy(root = Port(id, 0)), Port(id, 0))

      case LC.Lam(param, body) =>
        val (net1, lamId) = net.addNode(ICNode.Con)
        val paramPort = Port(lamId, 1)
        val bodyEnv = env.updated(param, paramPort)
        val (net2, bodyPort) = compileExpr(body, net1, bodyEnv)
        val net3 = net2.link(Port(lamId, 2), bodyPort)
        (net3.copy(root = Port(lamId, 0)), Port(lamId, 0))

      case LC.App(func, arg) =>
        val (net1, appId) = net.addNode(ICNode.Con)
        val (net2, funcPort) = compileExpr(func, net1, env)
        val (net3, argPort) = compileExpr(arg, net2, env)
        val net4 = net3.link(funcPort, Port(appId, 0))
        val net5 = net4.link(argPort, Port(appId, 1))
        (net5, Port(appId, 2))

      case LC.Let(name, value, body) =>
        // Desugar let to application of lambda
        compileExpr(LC.App(LC.Lam(name, body), value), net, env)

      case LC.Lit(n) =>
        // Represent literals as references
        val (net1, id) = net.addNode(ICNode.Ref(s"#$n"))
        (net1.copy(root = Port(id, 0)), Port(id, 0))

      case LC.Prim(op, args) =>
        val (net1, id) = net.addNode(ICNode.Ref(s"@$op"))
        // Simple: chain arguments
        var currentNet = net1
        var lastPort = Port(id, 0)
        args.zipWithIndex.foreach { case (arg, i) =>
          val (net2, argPort) = compileExpr(arg, currentNet, env)
          currentNet = net2.link(lastPort, argPort)
          lastPort = Port(id, i + 1)
        }
        (currentNet.copy(root = Port(id, 0)), Port(id, 0))

  /** Decompile IC net back to LC */
  def decompile(net: ICNet): LC =
    decompilePort(net, net.root, Set.empty)._1

  private def decompilePort(net: ICNet, port: Port, visited: Set[Port]): (LC, Set[Port]) =
    if visited.contains(port) then
      (LC.Var(s"_rec${port.nodeId}"), visited)
    else
      val newVisited = visited + port
      net.getNode(port.nodeId) match
        case Some(ICNode.Con) =>
          port.portNum match
            case 0 => // Lambda root
              val paramName = s"x${port.nodeId}"
              val bodyPort = net.getLink(Port(port.nodeId, 2))
              bodyPort match
                case Some(bp) =>
                  val (body, v2) = decompilePort(net, bp, newVisited)
                  (LC.Lam(paramName, body), v2)
                case None =>
                  (LC.Lam(paramName, LC.Var("_")), newVisited)
            case 1 => // Parameter
              (LC.Var(s"x${port.nodeId}"), newVisited)
            case 2 => // Body result - application
              val funcPort = net.getLink(Port(port.nodeId, 0))
              val argPort = net.getLink(Port(port.nodeId, 1))
              val (func, v1) = funcPort.map(p => decompilePort(net, p, newVisited)).getOrElse((LC.Var("_f"), newVisited))
              val (arg, v2) = argPort.map(p => decompilePort(net, p, v1)).getOrElse((LC.Var("_a"), v1))
              (LC.App(func, arg), v2)
            case _ => (LC.Var("_"), newVisited)

        case Some(ICNode.Ref(name)) =>
          if name.startsWith("#") then
            (LC.Lit(name.drop(1).toIntOption.getOrElse(0)), newVisited)
          else if name.startsWith("@") then
            (LC.Prim(name.drop(1), Nil), newVisited)
          else
            (LC.Var(name), newVisited)

        case Some(ICNode.Era) =>
          (LC.Var("_era"), newVisited)

        case Some(ICNode.Dup(_)) =>
          // Duplicator - represents sharing
          val port1 = net.getLink(Port(port.nodeId, 1))
          port1 match
            case Some(p) => decompilePort(net, p, newVisited)
            case None => (LC.Var("_dup"), newVisited)

        case None =>
          (LC.Var("_unknown"), newVisited)

// =============================================================================
// Typed Lambda Calculus
// =============================================================================

/**
 * Types for the simply-typed lambda calculus.
 */
enum LCType:
  case TVar(name: String)
  case TInt
  case TBool
  case TArrow(from: LCType, to: LCType)
  case TForall(param: String, body: LCType)

  def render: String = this match
    case TVar(n)       => n
    case TInt          => "Int"
    case TBool         => "Bool"
    case TArrow(f, t)  => s"(${f.render} -> ${t.render})"
    case TForall(p, b) => s"∀$p. ${b.render}"

/**
 * Typed Lambda Calculus terms.
 */
enum TypedLC:
  case TVar(name: String, ty: LCType)
  case TLam(param: String, paramTy: LCType, body: TypedLC)
  case TApp(func: TypedLC, arg: TypedLC)
  case TLet(name: String, ty: LCType, value: TypedLC, body: TypedLC)
  case TLit(value: Int)
  case TAnn(term: TypedLC, ty: LCType)

  def getType: LCType = this match
    case TVar(_, ty)       => ty
    case TLam(_, pty, b)   => LCType.TArrow(pty, b.getType)
    case TApp(f, _)        => f.getType match
      case LCType.TArrow(_, to) => to
      case _ => LCType.TVar("?")
    case TLet(_, _, _, b)  => b.getType
    case TLit(_)           => LCType.TInt
    case TAnn(_, ty)       => ty

// =============================================================================
// Type Checking as Xform
// =============================================================================

/**
 * Type checking transformation: Untyped LC ↔ Typed LC.
 */
object TypeChecker extends Xform[LC, TypedLC]:
  type Env = Map[String, LCType]

  def forward(term: Term[LC]): Term[TypedLC] = term match
    case Term.Done(lc) => infer(lc, Map.empty) match
      case Right(typed) => Term.Done(typed)
      case Left(err)    => Term.Hole(Some(err))
    case Term.Hole(l) => Term.Hole(l)

  def backward(term: Term[TypedLC]): Term[LC] = term match
    case Term.Done(typed) => Term.Done(erase(typed))
    case Term.Hole(l)     => Term.Hole(l)

  /** Infer type and produce typed term */
  def infer(lc: LC, env: Env): Either[String, TypedLC] = lc match
    case LC.Var(name) =>
      env.get(name) match
        case Some(ty) => Right(TypedLC.TVar(name, ty))
        case None     => Left(s"Unbound variable: $name")

    case LC.Lam(param, body) =>
      // For untyped lambda, assume parameter type is a fresh type variable
      val paramTy = LCType.TVar(s"t_$param")
      val newEnv = env.updated(param, paramTy)
      infer(body, newEnv).map(tb => TypedLC.TLam(param, paramTy, tb))

    case LC.App(func, arg) =>
      for
        tf <- infer(func, env)
        ta <- infer(arg, env)
      yield TypedLC.TApp(tf, ta)

    case LC.Let(name, value, body) =>
      for
        tv <- infer(value, env)
        tb <- infer(body, env.updated(name, tv.getType))
      yield TypedLC.TLet(name, tv.getType, tv, tb)

    case LC.Lit(n) =>
      Right(TypedLC.TLit(n))

    case LC.Prim(op, args) =>
      // Primitives get typed based on arity
      val typedArgs = args.map(a => infer(a, env))
      val errors = typedArgs.collect { case Left(e) => e }
      if errors.nonEmpty then Left(errors.mkString(", "))
      else
        val typed = typedArgs.collect { case Right(t) => t }
        // Wrap in application to primitive
        typed.foldLeft(TypedLC.TVar(op, LCType.TVar(s"prim_$op")): TypedLC) { (acc, arg) =>
          TypedLC.TApp(acc, arg)
        }.pipe(Right(_))

  /** Erase types to get back untyped LC */
  def erase(typed: TypedLC): LC = typed match
    case TypedLC.TVar(n, _)       => LC.Var(n)
    case TypedLC.TLam(p, _, b)    => LC.Lam(p, erase(b))
    case TypedLC.TApp(f, a)       => LC.App(erase(f), erase(a))
    case TypedLC.TLet(n, _, v, b) => LC.Let(n, erase(v), erase(b))
    case TypedLC.TLit(n)          => LC.Lit(n)
    case TypedLC.TAnn(t, _)       => erase(t)

  extension [A](a: A)
    def pipe[B](f: A => B): B = f(a)

// =============================================================================
// Dependent Pattern Matching
// =============================================================================

/**
 * Pattern for dependent pattern matching.
 */
enum Pattern:
  case PVar(name: String)
  case PCon(name: String, args: List[Pattern])
  case PLit(value: Int)
  case PWild

/**
 * A case in a pattern match.
 */
case class MatchCase(pattern: Pattern, body: LC)

/**
 * Pattern match expression.
 */
case class PatternMatch(scrutinee: LC, cases: List[MatchCase])

/**
 * Compile pattern matches to LC (Church encoding style).
 */
object PatternCompiler extends Xform[PatternMatch, LC]:
  def forward(term: Term[PatternMatch]): Term[LC] = term match
    case Term.Done(pm) => Term.Done(compile(pm))
    case Term.Hole(l)  => Term.Hole(l)

  def backward(term: Term[LC]): Term[PatternMatch] = term match
    case Term.Done(lc) => decompile(lc).map(Term.Done(_)).getOrElse(Term.Hole(Some("cannot decompile")))
    case Term.Hole(l)  => Term.Hole(l)

  /** Compile pattern match to LC */
  def compile(pm: PatternMatch): LC =
    pm.cases.foldRight(LC.Var("_matchError"): LC) { (matchCase, fallback) =>
      compileCase(pm.scrutinee, matchCase, fallback)
    }

  private def compileCase(scrutinee: LC, mc: MatchCase, fallback: LC): LC =
    mc.pattern match
      case Pattern.PVar(name) =>
        LC.Let(name, scrutinee, mc.body)

      case Pattern.PCon(conName, args) =>
        // Assume constructor is applied as: Con arg1 arg2 ...
        // Generate projections
        val projections = args.zipWithIndex.map { case (pat, i) =>
          pat match
            case Pattern.PVar(n) => (n, LC.App(LC.Var(s"proj_${conName}_$i"), scrutinee))
            case _ => (s"_$i", LC.App(LC.Var(s"proj_${conName}_$i"), scrutinee))
        }
        projections.foldRight(mc.body) { case ((name, proj), body) =>
          LC.Let(name, proj, body)
        }

      case Pattern.PLit(n) =>
        // if scrutinee == n then body else fallback
        LC.Prim("if", List(
          LC.Prim("eq", List(scrutinee, LC.Lit(n))),
          mc.body,
          fallback
        ))

      case Pattern.PWild =>
        mc.body

  /** Try to decompile LC back to pattern match (simplified) */
  def decompile(lc: LC): Option[PatternMatch] =
    // This is a simplified decompiler - full impl would recognize patterns
    lc match
      case LC.Let(name, scrutinee, body) =>
        Some(PatternMatch(scrutinee, List(MatchCase(Pattern.PVar(name), body))))
      case _ => None

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
