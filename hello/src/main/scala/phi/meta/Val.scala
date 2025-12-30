package phi.meta

/**
 * Val: Runtime values in Phi (generated from meta.phi)
 */
enum Val:
  case VCon(name: String, args: List[Val])  // Constructor application
  case VStr(s: String)                       // String literal
  case VInt(n: Int)                          // Integer literal
  case VList(elems: List[Val])               // List value

object Val:
  import Val.*
  
  extension (v: Val)
    def show: String = v match
      case VCon(name, Nil) => name
      case VCon(name, args) => s"$name(${args.map(_.show).mkString(", ")})"
      case VStr(s) => s"\"$s\""
      case VInt(n) => n.toString
      case VList(elems) => s"[${elems.map(_.show).mkString(", ")}]"

/**
 * Env: Variable bindings (generated from meta.phi)
 */
enum Env:
  case EmptyEnv
  case Bind(name: String, value: Val, rest: Env)

object Env:
  import Env.*
  
  def lookup(name: String, env: Env): Option[Val] = env match
    case EmptyEnv => None
    case Bind(n, v, rest) => if n == name then Some(v) else lookup(name, rest)
  
  def extend(name: String, value: Val, env: Env): Env = Bind(name, value, env)
  
  def fromMap(m: Map[String, Val]): Env =
    m.foldLeft[Env](EmptyEnv) { case (env, (k, v)) => Bind(k, v, env) }
  
  def toMap(env: Env): Map[String, Val] = env match
    case EmptyEnv => Map.empty
    case Bind(n, v, rest) => toMap(rest) + (n -> v)

/**
 * Pat: Patterns for matching (generated from meta.phi)
 */
enum Pat:
  case PVar(name: String)           // Variable pattern - matches anything, binds
  case PCon(name: String, args: List[Pat])  // Constructor pattern
  case PWild                        // Wildcard - matches anything, no binding
  case PLit(value: Val)             // Literal pattern

/**
 * Expr: Expressions to evaluate (generated from meta.phi)
 */
enum Expr:
  case EVar(name: String)                      // Variable reference
  case ECon(name: String, args: List[Expr])    // Constructor call
  case EApp(func: Expr, arg: Expr)             // Application
  case ELam(param: String, body: Expr)         // Lambda
  case ELet(name: String, value: Expr, body: Expr)  // Let binding
  case EMatch(scrutinee: Expr, cases: List[Expr])   // Pattern match
  case ECase(pattern: Pat, body: Expr)         // Case clause

/**
 * Result: Match/eval results (generated from meta.phi)
 */
enum Result:
  case ROk(value: Val, env: Env)   // Successful match with bindings
  case RFail                        // Match failure
