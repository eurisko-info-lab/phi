package phi

/**
 * Phi Meta-Interpreter: Execute Phi language specifications.
 * 
 * This interprets the Phi language definition syntax shown in PROMPT.md,
 * allowing Phi to define and run languages declaratively.
 */

// =============================================================================
// Core AST for Phi Language Specifications
// =============================================================================

/** A complete language specification */
case class LangSpec(
  name: String,
  sorts: List[Sort],
  constructors: List[Constructor],
  xforms: List[XformSpec],
  changes: List[ChangeSpec],
  rules: List[Rule],
  defs: List[Def],
  strategies: Map[String, Strat]
)

case class Sort(name: String)

case class Constructor(
  name: String,
  params: List[(Option[String], LangType)], // (name?, type)
  returnSort: String
)

enum LangType:
  case SortRef(name: String)
  case Arrow(from: LangType, to: LangType)
  case Product(left: LangType, right: LangType)
  case ListOf(elem: LangType)

case class XformSpec(name: String, source: String, target: String)
case class ChangeSpec(name: String, sort: String)

case class Rule(
  name: String,
  direction: RuleDir,
  cases: List[RuleCase]
)

enum RuleDir:
  case Forward, Backward, Both

case class RuleCase(
  lhs: Pat,
  rhs: Pat,
  guards: List[RuleGuard]
)

case class RuleGuard(
  varName: String,
  expr: Pat,
  expected: Pat
)

/** Pattern language */
enum Pat:
  case PVar(name: String)
  case PCon(name: String, args: List[Pat])
  case PApp(func: Pat, arg: Pat)
  case PSubst(body: Pat, varName: String, replacement: Pat) // body[varName := replacement]

case class Def(
  name: String,
  sort: Option[String],
  body: Pat
)

/** Rewriting strategies */
enum Strat:
  case Apply(ruleName: String)
  case Seq(first: Strat, second: Strat)
  case Choice(left: Strat, right: Strat)
  case Repeat(inner: Strat)
  case Id

// =============================================================================
// Runtime Values
// =============================================================================

/** A value in the interpreted language */
enum Val:
  case VCon(name: String, args: List[Val])
  
  /** Pretty print, with special handling for Church numerals */
  def show: String = this match
    case VCon(name, Nil) => name
    case VCon("succ", List(inner)) =>
      def count(v: Val, n: Int): String = v match
        case VCon("succ", List(i)) => count(i, n + 1)
        case VCon("zero", Nil) => n.toString
        case other => s"succ^$n(${other.show})"
      count(inner, 1)
    case VCon(name, args) =>
      s"$name(${args.map(_.show).mkString(", ")})"

// =============================================================================
// Meta-Interpreter
// =============================================================================

class LangInterpreter(spec: LangSpec):
  import Val.*
  import Pat.*
  
  // Index definitions by name
  private val defs: Map[String, Pat] = spec.defs.map(d => d.name -> d.body).toMap
  
  // Index rules by name  
  private val rules: Map[String, List[RuleCase]] = 
    spec.rules.map(r => r.name -> r.cases).toMap
  
  /** Instantiate a pattern with an environment to get a value */
  def instantiate(p: Pat, env: Map[String, Val]): Val = p match
    case PVar(name) =>
      env.getOrElse(name, defs.get(name).map(instantiate(_, env)).getOrElse(
        throw RuntimeException(s"Unbound: $name")))
    
    case PCon(name, Nil) =>
      // Nullary constructor - check if it's a definition to expand
      defs.get(name).map(instantiate(_, env)).getOrElse(VCon(name, Nil))
    
    case PCon(name, args) =>
      VCon(name, args.map(instantiate(_, env)))
    
    case PApp(func, arg) =>
      VCon("app", List(instantiate(func, env), instantiate(arg, env)))
    
    case PSubst(body, varName, replacement) =>
      val bodyVal = instantiate(body, env)
      val replVal = instantiate(replacement, env)
      substitute(bodyVal, varName, replVal)
  
  /** Substitute a variable in a value */
  def substitute(value: Val, varName: String, replacement: Val): Val = value match
    // If we hit a lam that binds the same variable, stop (shadowing)
    case VCon("lam", List(VCon(x, Nil), ty, body)) if x == varName =>
      value
    // If this is the variable itself (represented as nullary constructor), replace
    case VCon(name, Nil) if name == varName =>
      replacement
    // Otherwise recurse
    case VCon(name, args) =>
      VCon(name, args.map(substitute(_, varName, replacement)))
  
  /** Match a value against a pattern, returning bindings if successful */
  def matchPat(value: Val, pattern: Pat): Option[Map[String, Val]] = (value, pattern) match
    case (_, PVar(name)) =>
      Some(Map(name -> value))
    
    case (VCon(vn, vargs), PCon(pn, pargs)) if vn == pn && vargs.length == pargs.length =>
      vargs.zip(pargs).foldLeft(Option(Map.empty[String, Val])) {
        case (Some(acc), (v, p)) => matchPat(v, p).map(acc ++ _)
        case (None, _) => None
      }
    
    case (VCon("app", List(f, a)), PApp(pf, pa)) =>
      for
        fb <- matchPat(f, pf)
        ab <- matchPat(a, pa)
      yield fb ++ ab
    
    case _ => None
  
  /** Try to apply a rule at the root of a value */
  def applyRule(value: Val, cases: List[RuleCase]): Option[Val] =
    cases.view.flatMap { rc =>
      val matched = matchPat(value, rc.lhs)
      matched.flatMap { bindings =>
        // Check guards
        val guardsOk = rc.guards.forall { g =>
          try
            val exprVal = instantiate(g.expr, bindings)
            val expectedVal = instantiate(g.expected, bindings)
            exprVal == expectedVal
          catch case _: Exception => false
        }
        if guardsOk then
          rc.rhs match
            // Special handling for substitution patterns: body[x := v]
            // x is a meta-variable that bound to a VCon(varName, Nil)
            case PSubst(PVar(bodyVar), metaVar, PVar(replVar)) =>
              for
                bodyVal <- bindings.get(bodyVar)
                replVal <- bindings.get(replVar)
                // Get the actual variable name from what the meta-variable bound to
                varName <- bindings.get(metaVar) match
                  case Some(VCon(name, Nil)) => Some(name)
                  case _ => None
              yield substitute(bodyVal, varName, replVal)
            case _ =>
              Some(instantiate(rc.rhs, bindings))
        else None
      }
    }.headOption
  
  /** Apply rules anywhere in term (innermost/bottom-up) */
  def applyAnywhere(value: Val, cases: List[RuleCase]): Option[Val] = value match
    case VCon(name, args) =>
      // First try to reduce in subterms
      val reduced = args.indices.foldLeft(Option.empty[Val]) { (acc, i) =>
        acc.orElse {
          applyAnywhere(args(i), cases).map { newArg =>
            VCon(name, args.updated(i, newArg))
          }
        }
      }
      // Then try at root
      reduced.orElse(applyRule(value, cases))
  
  /** Result of running a strategy: value + step count */
  case class NormResult(value: Val, steps: Int)
  
  /** Run a strategy on a value */
  def runStrategy(strat: Strat, value: Val, maxSteps: Int = 100000): NormResult =
    var steps = 0
    
    def go(s: Strat, v: Val): Option[Val] = s match
      case Strat.Id => Some(v)
      
      case Strat.Apply(ruleName) =>
        rules.get(ruleName).flatMap(applyAnywhere(v, _))
      
      case Strat.Seq(first, second) =>
        go(first, v).flatMap(go(second, _))
      
      case Strat.Choice(left, right) =>
        go(left, v).orElse(go(right, v))
      
      case Strat.Repeat(inner) =>
        var cur = v
        var changed = true
        while changed && steps < maxSteps do
          go(inner, cur) match
            case Some(next) if next != cur =>
              cur = next
              steps += 1
              changed = true
            case _ =>
              changed = false
        Some(cur)
    
    NormResult(go(strat, value).getOrElse(value), steps)
  
  /** Evaluate a definition */
  def evalDef(name: String): Val =
    defs.get(name).map(instantiate(_, Map.empty)).getOrElse(
      throw RuntimeException(s"Unknown def: $name"))
  
  /** Normalize a value using the 'normalize' strategy */
  def normalize(value: Val): NormResult =
    spec.strategies.get("normalize").map(runStrategy(_, value)).getOrElse(NormResult(value, 0))

// =============================================================================
// DSL for Building Language Specs in Scala
// =============================================================================

object LangDSL:
  import Pat.*
  import Strat.*
  
  class Builder(name: String):
    private var sorts = List.empty[Sort]
    private var constructors = List.empty[Constructor]
    private var xforms = List.empty[XformSpec]
    private var changes = List.empty[ChangeSpec]
    private var rules = List.empty[Rule]
    private var defs = List.empty[Def]
    private var strategies = Map.empty[String, Strat]
    
    def sort(n: String): this.type = { sorts :+= Sort(n); this }
    
    def con(n: String, ret: String): this.type = 
      { constructors :+= Constructor(n, Nil, ret); this }
    
    def con(n: String, params: (Option[String], LangType)*)(ret: String): this.type =
      { constructors :+= Constructor(n, params.toList, ret); this }
    
    def xform(n: String, src: String, tgt: String): this.type =
      { xforms :+= XformSpec(n, src, tgt); this }
    
    def change(n: String, s: String): this.type =
      { changes :+= ChangeSpec(n, s); this }
    
    def rule(n: String, cases: RuleCase*): this.type =
      { rules :+= Rule(n, RuleDir.Both, cases.toList); this }
    
    def defn(n: String, body: Pat): this.type =
      { defs :+= Def(n, None, body); this }
    
    def strategy(n: String, s: Strat): this.type =
      { strategies += (n -> s); this }
    
    def build(): LangSpec = 
      LangSpec(name, sorts, constructors, xforms, changes, rules, defs, strategies)
  
  def language(name: String): Builder = Builder(name)
  
  // Pattern helpers
  def v(n: String): Pat = PVar(n)
  def c(n: String, args: Pat*): Pat = PCon(n, args.toList)
  def app(f: Pat, a: Pat): Pat = PApp(f, a)
  def subst(body: Pat, varName: String, repl: Pat): Pat = PSubst(body, varName, repl)
  
  // Rule case helper
  def cas(lhs: Pat, rhs: Pat, guards: RuleGuard*): RuleCase = 
    RuleCase(lhs, rhs, guards.toList)
  
  // Strategy helpers  
  def apply(n: String): Strat = Apply(n)
  def seq(a: Strat, b: Strat): Strat = Seq(a, b)
  def choice(a: Strat, b: Strat): Strat = Choice(a, b)
  def repeat(s: Strat): Strat = Repeat(s)
  def oneOf(ss: Strat*): Strat = ss.reduceLeft(Choice(_, _))
