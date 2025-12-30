package phi

/**
 * ╔═══════════════════════════════════════════════════════════════════════════╗
 * ║                     Φ-META: Meta-Language Runtime                         ║
 * ╠═══════════════════════════════════════════════════════════════════════════╣
 * ║  Evaluation, pattern matching, and environment for .phi specifications    ║
 * ╚═══════════════════════════════════════════════════════════════════════════╝
 *
 * This module provides the runtime semantics for evaluating expressions
 * that arise from .phi language specifications. It bridges the gap between
 * the abstract Core structures and concrete execution.
 *
 * KEY CONCEPTS
 * ============
 * 
 * Val     - Universal values (from Core)
 * Pat     - Patterns for destructuring values
 * Expr    - Expressions for constructing values
 * Env     - Name-to-value bindings
 * Result  - Evaluation outcome (success/failure)
 */
object Meta:
  import Core.*
  import Core.Val.*

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 1: Environments
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Environment: Name → Value bindings.
   * 
   * Implemented as a simple Map with helper methods for common operations.
   * In a production system, you might use a more efficient structure.
   */
  type Env = Map[String, Val]

  object Env:
    /** Empty environment */
    val empty: Env = Map.empty
    
    /** Create from varargs */
    def apply(bindings: (String, Val)*): Env = Map(bindings*)
    
    /** Create from map */
    def fromMap(m: Map[String, Val]): Env = m
    
    /** Look up a variable */
    def lookup(env: Env, name: String): Option[Val] = env.get(name)
    
    /** Extend with a new binding */
    def extend(env: Env, name: String, v: Val): Env = env + (name -> v)
    
    /** Extend with multiple bindings */
    def extendAll(env: Env, bindings: Env): Env = env ++ bindings

  // Extension methods for nicer syntax
  extension (env: Env)
    def lookup(name: String): Option[Val] = env.get(name)
    def extend(name: String, v: Val): Env = env + (name -> v)
    def extendAll(bindings: Env): Env = env ++ bindings

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 2: Patterns
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Pattern: For destructuring and matching values.
   * 
   * Patterns bind names to parts of values:
   *   PVar("x")                    -- bind anything to x
   *   PWild                        -- match anything, bind nothing
   *   PCon("Add", [PVar("l"), PVar("r")])  -- match Add(l, r)
   *   PLit(VInt(42))               -- match exactly 42
   *   PList([PVar("h"), PWild])    -- match 2-element list
   */
  enum Pat:
    case PVar(name: String)               // Variable: binds value to name
    case PWild                             // Wildcard: matches anything
    case PCon(name: String, args: List[Pat])  // Constructor pattern
    case PLit(value: Val)                  // Literal: exact match
    case PList(patterns: List[Pat])        // List pattern
    case POr(left: Pat, right: Pat)        // Alternative: p1 | p2
    case PAs(name: String, pattern: Pat)   // Named pattern: x @ p
    
    /**
     * Match this pattern against a value.
     * 
     * Returns Some(env) with bindings if match succeeds,
     * None if match fails.
     */
    def matchAgainst(v: Val): Option[Env] = this match
      case PVar(name) => Some(Env(name -> v))
      
      case PWild => Some(Env.empty)
      
      case PCon(name, args) => v match
        case VCon(n, vs) if n == name && vs.length == args.length =>
          val results = args.zip(vs).map((p, v) => p.matchAgainst(v))
          results.foldLeft(Some(Env.empty): Option[Env]) { 
            case (Some(acc), Some(env)) => Some(acc.extendAll(env))
            case _ => None
          }
        case _ => None
      
      case PLit(expected) =>
        if v == expected then Some(Env.empty) else None
      
      case PList(patterns) => v match
        case VList(elems) if elems.length == patterns.length =>
          val results = patterns.zip(elems).map((p, v) => p.matchAgainst(v))
          results.foldLeft(Some(Env.empty): Option[Env]) {
            case (Some(acc), Some(env)) => Some(acc.extendAll(env))
            case _ => None
          }
        case _ => None
      
      case POr(left, right) =>
        left.matchAgainst(v).orElse(right.matchAgainst(v))
      
      case PAs(name, pattern) =>
        pattern.matchAgainst(v).map(_.extend(name, v))
    
    /** Pretty-print this pattern */
    def show: String = this match
      case PVar(name) => name
      case PWild => "_"
      case PCon(name, Nil) => name
      case PCon(name, args) => s"$name(${args.map(_.show).mkString(", ")})"
      case PLit(v) => v.show
      case PList(ps) => s"[${ps.map(_.show).mkString(", ")}]"
      case POr(l, r) => s"(${l.show} | ${r.show})"
      case PAs(name, p) => s"$name @ ${p.show}"

  export Pat.*

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 3: Expressions
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Expression: For constructing and computing values.
   * 
   * Expressions are evaluated in an environment to produce values:
   *   EVar("x")                    -- lookup x
   *   ELit(VInt(42))               -- literal 42
   *   ECon("Add", [EVar("x"), EVar("y")])  -- construct Add(x, y)
   *   ELet("x", expr1, expr2)      -- let x = expr1 in expr2
   *   EMatch(expr, cases)          -- pattern match
   */
  enum Expr:
    case EVar(name: String)                     // Variable lookup
    case ELit(value: Val)                       // Literal value
    case ECon(name: String, args: List[Expr])   // Constructor
    case EList(elements: List[Expr])            // List construction
    case ELet(name: String, value: Expr, body: Expr)  // Let binding
    case EMatch(scrutinee: Expr, cases: List[(Pat, Expr)])  // Pattern match
    case EIf(cond: Expr, thenE: Expr, elseE: Expr)  // Conditional
    case EApp(func: Expr, arg: Expr)            // Function application
    case ELam(param: String, body: Expr)        // Lambda abstraction
    
    /**
     * Evaluate this expression in an environment.
     * 
     * Returns the resulting value, or throws on error.
     * For production use, return Result instead.
     */
    def eval(env: Env): Val = this match
      case EVar(name) => 
        env.lookup(name).getOrElse(throw new Error(s"Unbound: $name"))
      
      case ELit(v) => v
      
      case ECon(name, args) => 
        VCon(name, args.map(_.eval(env)))
      
      case EList(elems) => 
        VList(elems.map(_.eval(env)))
      
      case ELet(name, value, body) =>
        val v = value.eval(env)
        body.eval(env.extend(name, v))
      
      case EMatch(scrut, cases) =>
        val v = scrut.eval(env)
        cases.view
          .flatMap((pat, body) => pat.matchAgainst(v).map(bindings => 
            body.eval(env.extendAll(bindings))))
          .headOption
          .getOrElse(throw new Error(s"No matching case for: ${v.show}"))
      
      case EIf(cond, thenE, elseE) =>
        cond.eval(env) match
          case VCon("True", Nil)  => thenE.eval(env)
          case VCon("False", Nil) => elseE.eval(env)
          case v => throw new Error(s"Expected Bool, got: ${v.show}")
      
      case EApp(func, arg) =>
        (func.eval(env), arg.eval(env)) match
          case (VCon("Closure", List(VStr(param), bodyVal, envVal)), argV) =>
            // This is a simplified closure representation
            // In practice, you'd store the body as data, not Val
            throw new Error("Native closures not supported in this simplified version")
          case (f, a) => 
            throw new Error(s"Cannot apply ${f.show} to ${a.show}")
      
      case ELam(param, body) =>
        // Return a closure representation
        VCon("Lambda", List(VStr(param), VStr(body.show)))
    
    /** Safe evaluation returning Result */
    def evalSafe(env: Env): Result[Val] =
      try Result.Ok(eval(env))
      catch case e: Error => Result.Err(e.getMessage)
    
    /** Pretty-print this expression */
    def show: String = this match
      case EVar(name) => name
      case ELit(v) => v.show
      case ECon(name, Nil) => name
      case ECon(name, args) => s"$name(${args.map(_.show).mkString(", ")})"
      case EList(elems) => s"[${elems.map(_.show).mkString(", ")}]"
      case ELet(n, v, b) => s"let $n = ${v.show} in ${b.show}"
      case EMatch(s, cs) => s"match ${s.show} { ${cs.map((p,e) => s"${p.show} => ${e.show}").mkString("; ")} }"
      case EIf(c, t, e) => s"if ${c.show} then ${t.show} else ${e.show}"
      case EApp(f, a) => s"(${f.show} ${a.show})"
      case ELam(p, b) => s"\\$p -> ${b.show}"

  export Expr.*

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 4: Results
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Result: Evaluation outcome.
   * 
   * Either a successful value or an error message.
   * This is a simple Either-style type for evaluation results.
   */
  enum Result[+A]:
    case Ok(value: A)
    case Err(message: String)
    
    def map[B](f: A => B): Result[B] = this match
      case Ok(a)   => Ok(f(a))
      case Err(m)  => Err(m)
    
    def flatMap[B](f: A => Result[B]): Result[B] = this match
      case Ok(a)  => f(a)
      case Err(m) => Err(m)
    
    def getOrElse[A2 >: A](default: => A2): A2 = this match
      case Ok(a)  => a
      case Err(_) => default
    
    def toOption: Option[A] = this match
      case Ok(a)  => Some(a)
      case Err(_) => None

  export Result.{Ok, Err}

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 5: Convenience Methods
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Eval: Module for standalone evaluation functions.
   */
  object Eval:
    /** Evaluate an expression in the empty environment */
    def eval(e: Expr): Result[Val] = e.evalSafe(Env.empty)
    
    /** Evaluate with given bindings */
    def evalWith(e: Expr, bindings: (String, Val)*): Result[Val] =
      e.evalSafe(Env(bindings*))
    
    /** Match a pattern against a value */
    def matchPat(p: Pat, v: Val): Option[Env] = p.matchAgainst(v)

  /**
   * Build: DSL for constructing expressions.
   */
  object Build:
    /** Variable reference */
    def v(name: String) = EVar(name)
    
    /** Integer literal */
    def int(n: Int) = ELit(VInt(n))
    
    /** String literal */
    def str(s: String) = ELit(VStr(s))
    
    /** Boolean literals */
    val True = ECon("True", Nil)
    val False = ECon("False", Nil)
    
    /** Construct a constructor */
    def con(name: String, args: Expr*) = ECon(name, args.toList)
    
    /** Build a list */
    def list(elems: Expr*) = EList(elems.toList)
    
    /** Let binding */
    def let(name: String, value: Expr)(body: Expr) = ELet(name, value, body)
    
    /** Pattern match */
    def matchE(scrutinee: Expr)(cases: (Pat, Expr)*) = 
      EMatch(scrutinee, cases.toList)
    
    /** If-then-else */
    def ifE(cond: Expr, thenE: Expr, elseE: Expr) = EIf(cond, thenE, elseE)
    
    /** Lambda */
    def lam(param: String)(body: Expr) = ELam(param, body)
    
    /** Application */
    def app(f: Expr, a: Expr) = EApp(f, a)

  /**
   * Pattern: DSL for constructing patterns.
   */
  object Pattern:
    /** Variable pattern */
    def pv(name: String) = PVar(name)
    
    /** Wildcard pattern */
    val wild = PWild
    
    /** Constructor pattern */
    def pcon(name: String, args: Pat*) = PCon(name, args.toList)
    
    /** Literal pattern */
    def plit(v: Val) = PLit(v)
    
    /** List pattern */
    def plist(elems: Pat*) = PList(elems.toList)

// End of Meta object
