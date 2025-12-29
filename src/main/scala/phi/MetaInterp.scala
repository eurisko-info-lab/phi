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
  strategies: Map[String, RewriteStrategy],
  theorems: List[Theorem] = Nil,
  attributes: List[AttrSpec] = Nil,  // Attribute declarations
  attrEquations: List[AttrEquation] = Nil,  // Attribute computation rules
  parent: Option[String] = None  // For "extends" - parent language name
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

/** Attribute specification */
case class AttrSpec(
  name: String,
  attrType: LangType,
  flow: AttrFlow,
  defaultValue: Option[MetaPattern] = None
)

/** Attribute flow direction */
enum AttrFlow:
  case Inherited   // Flows from parent to children (down)
  case Synthesized // Flows from children to parent (up)

/** 
 * Attribute equation - defines how to compute an attribute.
 * 
 * Synthesized: attr type(Var(x)) = lookup(x, env)
 *   - Computes attribute for matching pattern
 * 
 * Inherited (for child): attr env(Lam(x, body)) for body = extend(env, x, freshType)
 *   - Computes inherited attribute passed to specific child
 */
case class AttrEquation(
  attrName: String,           // e.g., "type", "env"
  pattern: MetaPattern,       // e.g., Var(x), App(f, a)
  forChild: Option[String],   // For inherited: which child gets this value (e.g., "body")
  computation: MetaPattern    // e.g., lookup(x, env), extend(env, x, t)
)

case class Rule(
  name: String,
  direction: RuleDir,
  cases: List[RuleCase]
)

enum RuleDir:
  case Forward, Backward, Both

case class RuleCase(
  lhs: MetaPattern,
  rhs: MetaPattern,
  guards: List[RuleGuard]
)

case class RuleGuard(
  varName: String,
  expr: MetaPattern,
  expected: MetaPattern
)

/** Pattern language */
enum MetaPattern:
  case PVar(name: String)
  case PCon(name: String, args: List[MetaPattern])
  case PApp(func: MetaPattern, arg: MetaPattern)
  case PSubst(body: MetaPattern, varName: String, replacement: MetaPattern) // body[varName := replacement]

case class Def(
  name: String,
  sort: Option[String],
  body: MetaPattern
)

/** Theorem declaration (Abella-style) */
case class Theorem(
  name: String,
  signature: LangType,    // The proposition type
  proof: Option[MetaPattern] = None  // Optional proof term
)

/** Rewriting strategies */
enum RewriteStrategy:
  case Apply(ruleName: String)
  case Seq(first: RewriteStrategy, second: RewriteStrategy)
  case Choice(left: RewriteStrategy, right: RewriteStrategy)
  case Repeat(inner: RewriteStrategy)
  case All(inner: RewriteStrategy)  // Non-deterministic: return all results
  case Unify(left: MetaPattern, right: MetaPattern)  // Built-in unification
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
  import MetaPattern.*
  
  // Index definitions by name
  private val defs: Map[String, MetaPattern] = spec.defs.map(d => d.name -> d.body).toMap
  
  // Index rules by name  
  private val rules: Map[String, List[RuleCase]] = 
    spec.rules.map(r => r.name -> r.cases).toMap
  
  /** Instantiate a pattern with an environment to get a value */
  def instantiate(p: MetaPattern, env: Map[String, Val]): Val = p match
    case PVar(name) =>
      env.getOrElse(name, defs.get(name).map(instantiate(_, env)).getOrElse(
        throw RuntimeException(s"Unbound: $name")))
    
    case PCon(name, Nil) =>
      // Nullary constructor - check if it's a definition to expand
      defs.get(name).map(instantiate(_, env)).getOrElse(VCon(name, Nil))
    
    // Special handling for parse(grammar, text) - tokenize then apply parse rules
    case PCon("parse", List(grammarPat, textPat)) =>
      val grammarVal = instantiate(grammarPat, env)
      val textVal = instantiate(textPat, env)
      // Extract grammar name and text string
      (grammarVal, textVal) match
        case (VCon(grammarName, Nil), VCon(text, Nil)) =>
          val tokens = tokenize(text, grammarName)
          // Look for Parse<Grammar>.* rules and apply them
          val parsePrefix = s"Parse${grammarName.capitalize}"
          val parseRules = spec.rules.filter(_.name.startsWith(parsePrefix))
          if parseRules.nonEmpty then
            // Apply parsing rules exhaustively
            applyParseRules(tokens, parseRules)
          else
            // No parse rules found, return tokens
            tokens
        case _ =>
          VCon("parse", List(grammarVal, textVal))
    
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
  def matchPat(value: Val, pattern: MetaPattern): Option[Map[String, Val]] = (value, pattern) match
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

  // ===========================================================================
  // Pattern/Value Conversion Utilities
  // ===========================================================================

  /** Convert a pattern to a value, treating unbound variables as logic variables */
  def patternToVal(p: MetaPattern): Val = p match
    case PVar(name) => VCon(name, Nil)  // Variables become nullary constructors (logic vars)
    case PCon(name, args) => VCon(name, args.map(patternToVal))
    case PApp(f, a) => VCon("app", List(patternToVal(f), patternToVal(a)))
    case PSubst(body, v, repl) => 
      VCon("subst", List(patternToVal(body), VCon(v, Nil), patternToVal(repl)))
  
  private var freshCounter = 0
  
  /** Create fresh variables for a clause (alpha-renaming) */
  def freshenClause(clause: RuleCase): RuleCase =
    freshCounter += 1
    val suffix = s"_$freshCounter"
    
    def freshenPat(p: MetaPattern): MetaPattern = p match
      case PVar(name) => PVar(name + suffix)
      case PCon(name, args) => PCon(name, args.map(freshenPat))
      case PApp(f, a) => PApp(freshenPat(f), freshenPat(a))
      case PSubst(body, v, repl) => PSubst(freshenPat(body), v + suffix, freshenPat(repl))
    
    RuleCase(freshenPat(clause.lhs), freshenPat(clause.rhs), clause.guards.map { g =>
      RuleGuard(g.varName + suffix, freshenPat(g.expr), freshenPat(g.expected))
    })
  
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

  // ===========================================================================
  // Non-deterministic Evaluation (for λProlog-style backtracking)
  // ===========================================================================

  /** 
   * Apply a rule at the root, returning ALL matching results (non-deterministic).
   * This enables backtracking search over multiple rule matches.
   */
  def applyRuleAll(value: Val, cases: List[RuleCase]): LazyList[Val] =
    cases.to(LazyList).flatMap { rc =>
      matchPat(value, rc.lhs) match
        case Some(bindings) =>
          val guardsOk = rc.guards.forall { g =>
            try
              val exprVal = instantiate(g.expr, bindings)
              val expectedVal = instantiate(g.expected, bindings)
              exprVal == expectedVal
            catch case _: Exception => false
          }
          if guardsOk then
            rc.rhs match
              case PSubst(PVar(bodyVar), metaVar, PVar(replVar)) =>
                (for
                  bodyVal <- bindings.get(bodyVar)
                  replVal <- bindings.get(replVar)
                  varName <- bindings.get(metaVar) match
                    case Some(VCon(name, Nil)) => Some(name)
                    case _ => None
                yield substitute(bodyVal, varName, replVal)).to(LazyList)
              case _ =>
                LazyList(instantiate(rc.rhs, bindings))
          else LazyList.empty
        case None => LazyList.empty
    }

  /** Apply rules anywhere in term, returning all results */
  def applyAnywhereAll(value: Val, cases: List[RuleCase]): LazyList[Val] = value match
    case VCon(name, args) if name.endsWith(".forward") || name.endsWith(".backward") =>
      val argsAsTuple = args match
        case List(a) => a
        case List(a, b) => VCon("pair", List(a, b))
        case List(a, b, c) => VCon("pair", List(a, VCon("pair", List(b, c))))
        case _ => VCon("tuple", args)
      
      val directResults = applyRuleAll(argsAsTuple, cases)
      val xformCases = rules.getOrElse(name, Nil)
      val xformResults = if xformCases.nonEmpty && xformCases != cases then
        applyRuleAll(argsAsTuple, xformCases)
      else LazyList.empty
      
      val subResults = args.indices.to(LazyList).flatMap { i =>
        applyAnywhereAll(args(i), cases).map { newArg =>
          VCon(name, args.updated(i, newArg))
        }
      }
      
      directResults #::: xformResults #::: subResults
        
    case VCon(name, args) =>
      // Try in subterms first
      val subResults = args.indices.to(LazyList).flatMap { i =>
        applyAnywhereAll(args(i), cases).map { newArg =>
          VCon(name, args.updated(i, newArg))
        }
      }
      // Then try at root
      val constructorCases = cases.filter(_.lhs.isInstanceOf[PCon])
      val rootResults = applyRuleAll(value, constructorCases)
      subResults #::: rootResults

  /**
   * Run a strategy non-deterministically, returning all possible results.
   */
  def runStrategyAll(strat: RewriteStrategy, value: Val): LazyList[Val] = strat match
    case RewriteStrategy.Id => LazyList(value)
    
    case RewriteStrategy.Apply(ruleName) =>
      rules.get(ruleName).map(applyAnywhereAll(value, _)).getOrElse(LazyList.empty)
    
    case RewriteStrategy.Seq(first, second) =>
      runStrategyAll(first, value).flatMap(runStrategyAll(second, _))
    
    case RewriteStrategy.Choice(left, right) =>
      runStrategyAll(left, value) #::: runStrategyAll(right, value)
    
    case RewriteStrategy.All(inner) =>
      // All is identity in non-det context - already returns all results
      runStrategyAll(inner, value)
    
    case RewriteStrategy.Repeat(inner) =>
      // Non-det repeat: try 0, 1, 2, ... applications
      def repeatN(v: Val, n: Int, maxN: Int): LazyList[Val] =
        if n >= maxN then LazyList(v)
        else
          val oneStep = runStrategyAll(inner, v)
          if oneStep.isEmpty then LazyList(v)
          else v #:: oneStep.flatMap(next => repeatN(next, n + 1, maxN))
      repeatN(value, 0, 1000)
    
    case RewriteStrategy.Unify(left, right) =>
      // Built-in unification - returns value with substitutions applied
      val lVal = instantiate(left, Map.empty)
      val rVal = instantiate(right, Map.empty)
      unifyVals(lVal, rVal, Map.empty) match
        case Some(subst) => LazyList(applySubstToVal(value, subst))
        case None => LazyList.empty

  /** Unify two values, returning substitution if successful */
  def unifyVals(t1: Val, t2: Val, subst: Map[String, Val]): Option[Map[String, Val]] =
    val s1 = applySubstToVal(t1, subst)
    val s2 = applySubstToVal(t2, subst)
    
    (s1, s2) match
      case (VCon(n1, args1), VCon(n2, args2)) if n1 == n2 && args1.length == args2.length =>
        args1.zip(args2).foldLeft(Option(subst)) {
          case (Some(acc), (a1, a2)) => unifyVals(a1, a2, acc)
          case (None, _) => None
        }
      
      case (VCon(v, Nil), t) if isLogicVariable(v) =>
        Some(subst + (v -> t))
      
      case (t, VCon(v, Nil)) if isLogicVariable(v) =>
        Some(subst + (v -> t))
        
      case _ if s1 == s2 => Some(subst)
      case _ => None

  /** Check if a name is a logic variable (lowercase start or underscore) */
  def isLogicVariable(name: String): Boolean =
    name.headOption.exists(c => c.isLower || c == '_') && 
    name.forall(c => c.isLetterOrDigit || c == '_')

  /** Apply substitution to a value */
  def applySubstToVal(value: Val, subst: Map[String, Val]): Val = value match
    case VCon(name, Nil) if subst.contains(name) => applySubstToVal(subst(name), subst)
    case VCon(name, args) => VCon(name, args.map(applySubstToVal(_, subst)))
  
  /** Apply rules anywhere in term (innermost/bottom-up) */
  def applyAnywhere(value: Val, cases: List[RuleCase]): Option[Val] = value match
    case VCon(name, args) if name.endsWith(".forward") || name.endsWith(".backward") =>
      // Xform call - match the args as a tuple against the xform rules
      val argsAsTuple = args match
        case List(a) => a
        case List(a, b) => VCon("pair", List(a, b))
        case List(a, b, c) => VCon("pair", List(a, VCon("pair", List(b, c))))
        case _ => VCon("tuple", args)
      
      // Try to apply the provided cases (which should be xform rules)
      applyRule(argsAsTuple, cases).orElse {
        // Also try looking up rules by the xform name (for strategies that include multiple rules)
        val xformCases = rules.getOrElse(name, Nil)
        if xformCases.nonEmpty && xformCases != cases then
          applyRule(argsAsTuple, xformCases)
        else
          // Try to reduce inside the args
          val reduced = args.indices.foldLeft(Option.empty[Val]) { (acc, i) =>
            acc.orElse {
              applyAnywhere(args(i), cases).map { newArg =>
                VCon(name, args.updated(i, newArg))
              }
            }
          }
          reduced
      }
        
    case VCon(name, args) =>
      // Regular constructor - first try to reduce in subterms
      val reduced = args.indices.foldLeft(Option.empty[Val]) { (acc, i) =>
        acc.orElse {
          applyAnywhere(args(i), cases).map { newArg =>
            VCon(name, args.updated(i, newArg))
          }
        }
      }
      // Then try at root, but only with rules that have a constructor pattern at root
      // (not bare variable patterns, which would match anything - used in xform rules)
      val constructorCases = cases.filter { rc =>
        rc.lhs match
          case PCon(_, _) => true
          case _ => false
      }
      reduced.orElse(applyRule(value, constructorCases))
  
  /** Result of running a strategy: value + step count */
  case class NormResult(value: Val, steps: Int)
  
  /** Run a strategy on a value (deterministic - first result only) */
  def runStrategy(strat: RewriteStrategy, value: Val, maxSteps: Int = 100000): NormResult =
    var steps = 0
    
    def go(s: RewriteStrategy, v: Val): Option[Val] = s match
      case RewriteStrategy.Id => Some(v)
      
      case RewriteStrategy.Apply(ruleName) =>
        rules.get(ruleName).flatMap(applyAnywhere(v, _))
      
      case RewriteStrategy.Seq(first, second) =>
        go(first, v).flatMap(go(second, _))
      
      case RewriteStrategy.Choice(left, right) =>
        go(left, v).orElse(go(right, v))
      
      case RewriteStrategy.Repeat(inner) =>
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
      
      case RewriteStrategy.All(inner) =>
        // In deterministic mode, All just returns the first result
        go(inner, v)
      
      case RewriteStrategy.Unify(left, right) =>
        val lVal = instantiate(left, Map.empty)
        val rVal = instantiate(right, Map.empty)
        unifyVals(lVal, rVal, Map.empty).map(_ => v)
    
    NormResult(go(strat, value).getOrElse(value), steps)
  
  /** Evaluate a definition */
  def evalDef(name: String): Val =
    defs.get(name).map(instantiate(_, Map.empty)).getOrElse(
      throw RuntimeException(s"Unknown def: $name"))
  
  /** Normalize a value using the 'normalize' strategy */
  def normalize(value: Val): NormResult =
    spec.strategies.get("normalize").map(runStrategy(_, value)).getOrElse(NormResult(value, 0))
  
  /** 
   * Run a logic query using unification-based matching.
   * This enables λProlog-style backtracking search.
   * @param goal the goal to solve
   * @param maxSolutions maximum number of solutions to return
   * @return list of result values
   */
  def query(goal: Val, maxSolutions: Int = 10): List[Val] =
    val allCases = rules.values.flatten.toList
    solveGoal(goal, allCases).take(maxSolutions).toList
  
  /** Run a query on a specific set of rules */
  def queryWith(goal: Val, ruleNames: List[String], maxSolutions: Int = 10): List[Val] =
    val cases = ruleNames.flatMap(rules.get).flatten
    solveGoal(goal, cases).take(maxSolutions).toList
  
  /** 
   * Solve a goal using unification-based matching with backtracking.
   * Returns all possible RHS values that the goal can unify to.
   */
  def solveGoal(goal: Val, cases: List[RuleCase], subst: Map[String, Val] = Map.empty): LazyList[Val] =
    val g = applySubstToVal(goal, subst)
    
    g match
      // Built-in True succeeds
      case VCon("True", Nil) => LazyList(g)
      case VCon("true", Nil) => LazyList(g)
      
      // Built-in False fails
      case VCon("False", Nil) => LazyList.empty
      case VCon("false", Nil) => LazyList.empty
      
      // Conjunction
      case VCon("And", List(g1, g2)) =>
        for
          r1 <- solveGoal(g1, cases, subst)
          r2 <- solveGoal(g2, cases, subst)
        yield VCon("And", List(r1, r2))
      
      // Disjunction
      case VCon("Or", List(g1, g2)) =>
        solveGoal(g1, cases, subst) #::: solveGoal(g2, cases, subst)
      
      // Try to unify with each rule's LHS
      case _ =>
        cases.to(LazyList).flatMap { rc =>
          // Freshen clause variables
          val fresh = freshenClause(rc)
          val lhsVal = patternToVal(fresh.lhs)
          
          // Unify goal with LHS
          unifyVals(g, lhsVal, subst) match
            case Some(newSubst) =>
              // Success! Return the RHS with substitution applied
              val rhsVal = patternToVal(fresh.rhs)
              LazyList(applySubstToVal(rhsVal, newSubst))
            case None =>
              LazyList.empty
        }
  
  /** Run a strategy and return all results (non-deterministic) */
  def queryStrategy(strat: RewriteStrategy, value: Val, maxSolutions: Int = 10): List[Val] =
    runStrategyAll(strat, value).take(maxSolutions).toList

  // ===========================================================================
  // Attribute Evaluation
  // ===========================================================================
  
  /** Index attribute equations by (attrName, constructorName) for fast lookup */
  private val attrEqIndex: Map[(String, String), List[AttrEquation]] =
    spec.attrEquations.groupBy { eq =>
      val conName = eq.pattern match
        case PCon(name, _) => name
        case _ => ""
      (eq.attrName, conName)
    }
  
  /** Index attribute declarations by name */
  private val attrDefs: Map[String, AttrSpec] =
    spec.attributes.map(a => a.name -> a).toMap
  
  /** 
   * Attributed value - a Val with computed attributes.
   * inherited: attributes passed down from parent
   * synthesized: attributes computed from children
   */
  case class AttrVal(
    value: Val,
    inherited: Map[String, Val] = Map.empty,
    synthesized: Map[String, Val] = Map.empty
  ):
    def attr(name: String): Option[Val] =
      synthesized.get(name).orElse(inherited.get(name))
  
  /**
   * Evaluate all attributes on a term tree.
   * 
   * @param value The term to evaluate attributes on
   * @param inherited Initial inherited attributes (from parent)
   * @return The attributed term with all attributes computed
   */
  def evalAttributes(value: Val, inherited: Map[String, Val] = Map.empty): AttrVal =
    value match
      case VCon(name, args) =>
        // 1. Find applicable equations for synthesized attributes
        val synthEqs = spec.attributes.filter(_.flow == AttrFlow.Synthesized).flatMap { attrSpec =>
          attrEqIndex.get((attrSpec.name, name))
        }.flatten
        
        // 2. Find applicable equations for inherited attributes to pass to children
        val inhEqs = spec.attributes.filter(_.flow == AttrFlow.Inherited).flatMap { attrSpec =>
          attrEqIndex.get((attrSpec.name, name))
        }.flatten
        
        // 3. Try to match the value against equations and extract bindings
        def findBindings(eq: AttrEquation): Option[Map[String, Val]] =
          matchPat(value, eq.pattern)
        
        // 4. Compute inherited attributes for each child
        def computeChildInherited(childIndex: Int, childVal: Val, childVar: Option[String]): Map[String, Val] =
          // Start with parent's inherited attributes
          var childInh = inherited
          
          // Add any equations that target this specific child (via "for childVar")
          for
            eq <- inhEqs
            forVar <- eq.forChild
            if childVar.contains(forVar) || (childVar.isEmpty && args.length == 1)
            bindings <- findBindings(eq)
          do
            // Also make inherited attrs available in bindings
            val fullBindings = bindings ++ inherited.map { case (k, v) => k -> v }
            val computed = instantiate(eq.computation, fullBindings)
            childInh = childInh.updated(eq.attrName, computed)
          
          childInh
        
        // 5. Recursively evaluate children with their inherited attributes
        // Extract child variable names from matching equation pattern
        val childVars: List[Option[String]] = synthEqs.headOption.orElse(inhEqs.headOption).map { eq =>
          eq.pattern match
            case PCon(_, patArgs) => patArgs.map {
              case PVar(v) => Some(v)
              case _ => None
            }
            case _ => args.map(_ => None)
        }.getOrElse(args.map(_ => None))
        
        val evaluatedChildren = args.zipWithIndex.map { case (child, idx) =>
          val childVar = childVars.lift(idx).flatten
          val childInh = computeChildInherited(idx, child, childVar)
          evalAttributes(child, childInh)
        }
        
        // 6. Compute synthesized attributes
        var synth = Map.empty[String, Val]
        for
          eq <- synthEqs
          bindings <- findBindings(eq)
        do
          // Build environment with:
          // - Pattern bindings
          // - Inherited attributes
          // - Child synthesized attributes (via attr(child) references)
          var env = bindings ++ inherited.map { case (k, v) => k -> v }
          
          // Add child attributes to environment
          // For each binding that's a child, add its synthesized attrs as attrName_childVar
          childVars.zip(evaluatedChildren).foreach { case (varOpt, childAttrVal) =>
            varOpt.foreach { v =>
              childAttrVal.synthesized.foreach { case (attrName, attrVal) =>
                // Make child's synthesized attrs available as attrName(childVar) calls
                // We'll handle this in instantiate via special "attr call" form
                env = env.updated(s"${attrName}_$v", attrVal)
              }
            }
          }
          
          val computed = instantiateWithAttrs(eq.computation, env, evaluatedChildren, childVars)
          synth = synth.updated(eq.attrName, computed)
        
        AttrVal(VCon(name, evaluatedChildren.map(_.value)), inherited, synth)
  
  /**
   * Instantiate a computation pattern, handling attribute references.
   * 
   * Attribute references like type(f) are resolved by looking up the
   * synthesized attribute "type" on the child bound to "f".
   */
  private def instantiateWithAttrs(
    p: MetaPattern,
    env: Map[String, Val],
    children: List[AttrVal],
    childVars: List[Option[String]]
  ): Val = p match
    case PVar(name) =>
      env.getOrElse(name, defs.get(name).map(instantiate(_, env)).getOrElse(
        throw RuntimeException(s"Unbound: $name")))
    
    // Attribute call: attrName(childVar) -> lookup childVar's synthesized attr
    case PCon(attrName, List(PVar(childVar))) if attrDefs.contains(attrName) =>
      // Find the child bound to childVar and get its attribute
      childVars.zip(children).find(_._1.contains(childVar)) match
        case Some((_, childAttrVal)) =>
          childAttrVal.synthesized.getOrElse(attrName,
            throw RuntimeException(s"Attribute $attrName not found on $childVar"))
        case None =>
          // Not a child reference - try as regular constructor
          env.get(s"${attrName}_$childVar").getOrElse(
            VCon(attrName, List(instantiateWithAttrs(PVar(childVar), env, children, childVars)))
          )
    
    case PCon(name, args) =>
      VCon(name, args.map(instantiateWithAttrs(_, env, children, childVars)))
    
    case PApp(func, arg) =>
      VCon("app", List(
        instantiateWithAttrs(func, env, children, childVars),
        instantiateWithAttrs(arg, env, children, childVars)
      ))
    
    case PSubst(body, varName, replacement) =>
      val bodyVal = instantiateWithAttrs(body, env, children, childVars)
      val replVal = instantiateWithAttrs(replacement, env, children, childVars)
      substitute(bodyVal, varName, replVal)

  
  /** Apply parsing rules exhaustively until no more match */
  def applyParseRules(tokens: Val, parseRules: List[Rule]): Val =
    var current = tokens
    var changed = true
    var steps = 0
    val maxSteps = 10000
    
    // Collect all rule cases
    val allCases = parseRules.flatMap(_.cases)
    
    while changed && steps < maxSteps do
      changed = false
      applyRule(current, allCases) match
        case Some(next) =>
          current = next
          changed = true
          steps += 1
        case None => ()
    
    current

  /** Tokenize a string into a token list for the given grammar */
  def tokenize(text: String, grammar: String): Val =
    import Val.*
    
    // Keywords by grammar
    val keywords = grammar match
      case "phi" | "Phi" => Set("language", "sort", "constructor", "xform", "change", 
                                 "rule", "def", "strategy", "where", "and")
      case "program" | "goal" => Set()  // Prolog has no keywords
      case _ => Set.empty[String]
    
    // Symbols to recognize
    val symbols = Map(
      "→" -> "arrow", "->" -> "arrow",
      "↦" -> "mapsto", "|->" -> "mapsto", 
      ":=" -> "assign", "=" -> "eq",
      "⇄" -> "biarrow", "<->" -> "biarrow",
      ":-" -> "turnstile",
      "×" -> "times", "*" -> "star",
      "|" -> "bar", "::" -> "cons",
      "≠" -> "neq", "!=" -> "neq",
      "++" -> "concat"
    )
    
    // Simple tokenizer
    def tok(s: String): List[Val] =
      val trimmed = s.trim
      if trimmed.isEmpty then Nil
      else
        // Skip comments
        val noComment = if trimmed.startsWith("//") then 
          trimmed.dropWhile(_ != '\n').drop(1)
        else trimmed
        
        val t = noComment.trim
        if t.isEmpty then Nil
        else if t.startsWith("(") then VCon("TokLPar", Nil) :: tok(t.tail)
        else if t.startsWith(")") then VCon("TokRPar", Nil) :: tok(t.tail)
        else if t.startsWith("{") then VCon("TokLBrace", Nil) :: tok(t.tail)
        else if t.startsWith("}") then VCon("TokRBrace", Nil) :: tok(t.tail)
        else if t.startsWith("[") then VCon("TokLBra", Nil) :: tok(t.tail)
        else if t.startsWith("]") then VCon("TokRBra", Nil) :: tok(t.tail)
        else if t.startsWith(",") then VCon("TokComma", Nil) :: tok(t.tail)
        else if t.startsWith(":") && !t.startsWith(":-") && !t.startsWith("::") && !t.startsWith(":=") then 
          VCon("TokColon", Nil) :: tok(t.tail)
        else if t.startsWith(".") then VCon("TokDot", Nil) :: tok(t.tail)
        else 
          // Check for multi-char symbols
          val symMatch = symbols.keys.find(t.startsWith)
          symMatch match
            case Some(sym) =>
              VCon("TokSym", List(VCon(symbols(sym), Nil))) :: tok(t.drop(sym.length))
            case None =>
              // Identifier or number
              if t.head.isDigit then
                val (num, rest) = t.span(_.isDigit)
                VCon("TokNum", List(numToVal(num.toInt))) :: tok(rest)
              else if t.head.isLetter || t.head == '_' then
                val (id, rest) = t.span(c => c.isLetterOrDigit || c == '_')
                val tokType = if keywords.contains(id) then "TokKw" else "TokId"
                VCon(tokType, List(VCon(id, Nil))) :: tok(rest)
              else
                // Skip unknown char
                tok(t.tail)
    
    def numToVal(n: Int): Val =
      if n == 0 then VCon("zero", Nil)
      else VCon("succ", List(numToVal(n - 1)))
    
    // Build cons list from tokens
    def toList(tokens: List[Val]): Val = tokens match
      case Nil => VCon("nil", Nil)
      case h :: t => VCon("cons", List(h, toList(t)))
    
    toList(tok(text) :+ VCon("TokEOF", Nil))

// =============================================================================
// DSL for Building Language Specs in Scala
// =============================================================================

object LangDSL:
  import MetaPattern.*
  import RewriteStrategy.*
  
  class Builder(name: String):
    private var sorts = List.empty[Sort]
    private var constructors = List.empty[Constructor]
    private var xforms = List.empty[XformSpec]
    private var changes = List.empty[ChangeSpec]
    private var rules = List.empty[Rule]
    private var defs = List.empty[Def]
    private var strategies = Map.empty[String, RewriteStrategy]
    
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
    
    def defn(n: String, body: MetaPattern): this.type =
      { defs :+= Def(n, None, body); this }
    
    def strategy(n: String, s: RewriteStrategy): this.type =
      { strategies += (n -> s); this }
    
    def build(): LangSpec = 
      LangSpec(name, sorts, constructors, xforms, changes, rules, defs, strategies)
  
  def language(name: String): Builder = Builder(name)
  
  // Pattern helpers
  def v(n: String): MetaPattern = PVar(n)
  def c(n: String, args: MetaPattern*): MetaPattern = PCon(n, args.toList)
  def app(f: MetaPattern, a: MetaPattern): MetaPattern = PApp(f, a)
  def subst(body: MetaPattern, varName: String, repl: MetaPattern): MetaPattern = PSubst(body, varName, repl)
  
  // Rule case helper
  def cas(lhs: MetaPattern, rhs: MetaPattern, guards: RuleGuard*): RuleCase = 
    RuleCase(lhs, rhs, guards.toList)
  
  // Strategy helpers  
  def apply(n: String): RewriteStrategy = Apply(n)
  def seq(a: RewriteStrategy, b: RewriteStrategy): RewriteStrategy = Seq(a, b)
  def choice(a: RewriteStrategy, b: RewriteStrategy): RewriteStrategy = Choice(a, b)
  def repeat(s: RewriteStrategy): RewriteStrategy = Repeat(s)
  def oneOf(ss: RewriteStrategy*): RewriteStrategy = ss.reduceLeft(Choice(_, _))
