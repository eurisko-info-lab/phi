package phi

/**
 * Validation for Phi Language Specifications.
 * 
 * Performs semantic validation after parsing to catch errors
 * that the parser cannot detect syntactically.
 */

// =============================================================================
// Validation Result Types
// =============================================================================

enum ValidationSeverity:
  case Error   // Must fix - will cause runtime failure
  case Warning // Should fix - potential issue

case class ValidationIssue(
  severity: ValidationSeverity,
  category: String,
  message: String,
  location: Option[String] = None  // e.g., "rule Beta.forward", "def test"
)

case class ValidationResult(
  issues: List[ValidationIssue]
):
  def errors: List[ValidationIssue] = 
    issues.filter(_.severity == ValidationSeverity.Error)
  
  def warnings: List[ValidationIssue] = 
    issues.filter(_.severity == ValidationSeverity.Warning)
  
  def isValid: Boolean = errors.isEmpty
  
  def hasWarnings: Boolean = warnings.nonEmpty
  
  def ++(other: ValidationResult): ValidationResult =
    ValidationResult(issues ++ other.issues)
  
  def summary: String =
    val errs = errors.size
    val warns = warnings.size
    if errs == 0 && warns == 0 then "✓ Valid"
    else if errs == 0 then s"✓ Valid with $warns warning(s)"
    else s"✗ Invalid: $errs error(s), $warns warning(s)"

object ValidationResult:
  val empty: ValidationResult = ValidationResult(Nil)
  
  def error(category: String, message: String, location: Option[String] = None): ValidationResult =
    ValidationResult(List(ValidationIssue(ValidationSeverity.Error, category, message, location)))
  
  def warning(category: String, message: String, location: Option[String] = None): ValidationResult =
    ValidationResult(List(ValidationIssue(ValidationSeverity.Warning, category, message, location)))

// =============================================================================
// Validator
// =============================================================================

object LangValidator:
  
  /** Run all validation checks on a language specification */
  def validate(spec: LangSpec): ValidationResult =
    List(
      checkDuplicates(spec),
      checkUndefinedSortRefs(spec),
      checkUndefinedConstructorRefs(spec),
      checkUndefinedXformRefs(spec),
      checkUndefinedRuleRefs(spec),
      checkUndefinedDefRefs(spec),
      checkUnusedSorts(spec),
      checkUnusedConstructors(spec),
      checkUnusedXforms(spec),
      checkUnusedRules(spec),
      checkRulePatternVariables(spec),
      checkConstructorArity(spec),
      checkStrategyRefs(spec),
      checkAttributeRefs(spec)
    ).foldLeft(ValidationResult.empty)(_ ++ _)
  
  // ===========================================================================
  // Duplicate Checks
  // ===========================================================================
  
  private def checkDuplicates(spec: LangSpec): ValidationResult =
    val sortDups = findDuplicates(spec.sorts.map(_.name))
    val conDups = findDuplicates(spec.constructors.map(_.name))
    val xformDups = findDuplicates(spec.xforms.map(_.name))
    val defDups = findDuplicates(spec.defs.map(_.name))
    val ruleDups = findDuplicates(spec.rules.map(_.name))
    val attrDups = findDuplicates(spec.attributes.map(_.name))
    
    val issues = 
      sortDups.map(n => ValidationIssue(ValidationSeverity.Error, "duplicate", s"Duplicate sort: $n")) ++
      conDups.map(n => ValidationIssue(ValidationSeverity.Error, "duplicate", s"Duplicate constructor: $n")) ++
      xformDups.map(n => ValidationIssue(ValidationSeverity.Error, "duplicate", s"Duplicate xform: $n")) ++
      defDups.map(n => ValidationIssue(ValidationSeverity.Error, "duplicate", s"Duplicate definition: $n")) ++
      ruleDups.map(n => ValidationIssue(ValidationSeverity.Error, "duplicate", s"Duplicate rule: $n")) ++
      attrDups.map(n => ValidationIssue(ValidationSeverity.Error, "duplicate", s"Duplicate attribute: $n"))
    
    ValidationResult(issues)
  
  private def findDuplicates[A](items: List[A]): List[A] =
    items.groupBy(identity).filter(_._2.size > 1).keys.toList
  
  // ===========================================================================
  // Undefined Reference Checks
  // ===========================================================================
  
  // Extract base sort name from a potentially parameterized sort like "Option[A]" -> "Option"
  private def baseSortName(s: String): String =
    val bracketIdx = s.indexOf('[')
    if bracketIdx > 0 then s.substring(0, bracketIdx) else s
  
  // Extract type parameters from a parameterized name like "Foo[A,B,C]" -> List("A", "B", "C")
  private def extractTypeParams(s: String): List[String] =
    val bracketIdx = s.indexOf('[')
    if bracketIdx > 0 && s.endsWith("]") then
      s.substring(bracketIdx + 1, s.length - 1).split(",").map(_.trim).toList
    else Nil
  
  // Collect all type parameter names defined in the spec
  // From sorts, constructors, xforms, and changes (all can have [A,B,...] params)
  private def collectTypeParams(spec: LangSpec): Set[String] =
    val fromSorts = spec.sorts.flatMap(_.typeParams)
    val fromConstructors = spec.constructors.flatMap(c => extractTypeParams(c.name))
    val fromXforms = spec.xforms.flatMap(x => extractTypeParams(x.name))
    val fromChanges = spec.changes.flatMap(c => extractTypeParams(c.name))
    (fromSorts ++ fromConstructors ++ fromXforms ++ fromChanges).toSet
  
  private def checkUndefinedSortRefs(spec: LangSpec): ValidationResult =
    val definedSorts = spec.sorts.map(_.name).toSet ++ builtinSorts
    val typeParams = collectTypeParams(spec)
    
    // Collect all sort references
    val sortRefs = scala.collection.mutable.ListBuffer[(String, String)]() // (sort, location)
    
    // From constructors (return type and param types)
    spec.constructors.foreach { con =>
      sortRefs += ((con.returnSort, s"constructor ${con.name}"))
      con.params.foreach { case (_, ty) =>
        collectSortRefs(ty).foreach(s => sortRefs += ((s, s"constructor ${con.name}")))
      }
    }
    
    // From xforms (source and target) - parse type expressions
    spec.xforms.foreach { x =>
      parseTypeString(x.source).foreach(s => sortRefs += ((s, s"xform ${x.name}")))
      parseTypeString(x.target).foreach(s => sortRefs += ((s, s"xform ${x.name}")))
    }
    
    // From changes - parse type expression (change sort can be product types, etc.)
    spec.changes.foreach { c =>
      parseTypeString(c.sort).foreach(s => sortRefs += ((s, s"change ${c.name}")))
    }
    
    // From defs (if sort annotation present)
    spec.defs.foreach { d =>
      d.sort.foreach(s => sortRefs += ((s, s"def ${d.name}")))
    }
    
    // Filter: only report undefined if:
    // 1. Base sort name is not defined
    // 2. AND it's not a type parameter (explicit or single uppercase letter convention)
    def isTypeParam(s: String): Boolean =
      typeParams.contains(s) || (s.length == 1 && s.head.isUpper)
    
    val undefined = sortRefs.toList.filter { case (s, _) => 
      val base = baseSortName(s)
      !definedSorts.contains(base) && !isTypeParam(s)
    }
    val issues = undefined.map { case (s, loc) =>
      ValidationIssue(ValidationSeverity.Error, "undefined-sort", s"Undefined sort: $s", Some(loc))
    }
    
    ValidationResult(issues)
  
  private def collectSortRefs(ty: LangType): List[String] = ty match
    case LangType.SortRef(name) => 
      // Skip complex expressions that look like types but aren't sort names
      if name.contains("(") || name.contains("×") || name.contains("→") then Nil
      else List(name)
    case LangType.TypeApp(base, args) => 
      // Collect the base sort and all argument sorts
      base :: args.flatMap(collectSortRefs)
    case LangType.TypeVar(name) => 
      // Type variables are not sort references (they're parameters)
      Nil
    case LangType.Arrow(from, to) => collectSortRefs(from) ++ collectSortRefs(to)
    case LangType.Product(l, r) => collectSortRefs(l) ++ collectSortRefs(r)
    case LangType.ListOf(elem) => collectSortRefs(elem)
  
  private def checkUndefinedConstructorRefs(spec: LangSpec): ValidationResult =
    val definedCons = spec.constructors.map(_.name).toSet ++ builtinConstructors
    
    // Collect all constructor uses from patterns
    val conRefs = scala.collection.mutable.ListBuffer[(String, String)]()
    
    spec.rules.foreach { rule =>
      rule.cases.foreach { c =>
        collectPatternConstructors(c.lhs).foreach(n => conRefs += ((n, s"rule ${rule.name} LHS")))
        collectPatternConstructors(c.rhs).foreach(n => conRefs += ((n, s"rule ${rule.name} RHS")))
      }
    }
    
    spec.defs.foreach { d =>
      collectPatternConstructors(d.body).foreach(n => conRefs += ((n, s"def ${d.name}")))
    }
    
    // Common type/pattern variable names that shouldn't be warned about
    val commonPatternVars = Set(
      // Single uppercase letters (type variables)
      "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
      "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
      // Common primitive/builtin names
      "Int", "String", "Bool", "True", "False", "Unit", "Void", "Any", "Nothing",
      "Zero", "Succ", "Nil", "Cons", "None", "Some",
      // Common logic/algebra names
      "And", "Or", "Not", "Implies", "Iff", "Forall", "Exists",
      // Common term/type names used in examples
      "Term", "Type", "Expr", "Stmt", "Decl", "Pat", "Val",
      "Tiny", "AddZero", "AddSucc",
      // Common theorem names used as patterns
      "Beck", "Brown", "Density", "Coherence", "Mitchell", "SmallObject", "GabrielUlmer"
    )
    
    // Filter out pattern variables (lowercase), xform calls (contain '.'), and common pattern vars
    val actualConRefs = conRefs.toList.filter { case (name, _) =>
      name.headOption.exists(_.isUpper) && 
      !name.contains('.') &&
      !commonPatternVars.contains(name)
    }
    
    val undefined = actualConRefs.filter { case (n, _) => !definedCons.contains(n) }
    val issues = undefined.map { case (n, loc) =>
      ValidationIssue(ValidationSeverity.Warning, "undefined-constructor", 
        s"Possibly undefined constructor: $n (could be free identifier)", Some(loc))
    }
    
    ValidationResult(issues)
  
  // Generic pattern folder - extracts values from MetaPattern tree
  private def foldPattern[A](pat: MetaPattern)(
    onVar: String => List[A],
    onCon: (String, List[MetaPattern]) => List[A]
  ): List[A] = pat match
    case MetaPattern.PVar(name) => onVar(name)
    case MetaPattern.PCon(name, args) => 
      onCon(name, args) ++ args.flatMap(foldPattern(_)(onVar, onCon))
    case MetaPattern.PApp(f, a) => 
      foldPattern(f)(onVar, onCon) ++ foldPattern(a)(onVar, onCon)
    case MetaPattern.PSubst(body, _, repl) => 
      foldPattern(body)(onVar, onCon) ++ foldPattern(repl)(onVar, onCon)
  
  private def collectPatternConstructors(pat: MetaPattern): List[String] =
    foldPattern(pat)(
      onVar = _ => Nil,
      onCon = (name, _) => List(name)
    )
  
  private def checkUndefinedXformRefs(spec: LangSpec): ValidationResult =
    // Use base names (strip type params) for matching
    val definedXforms = spec.xforms.map(x => baseSortName(x.name)).toSet ++ 
                        spec.changes.map(_.name).toSet
    
    // Collect xform references from rule names (e.g., "Beta.forward" but NOT "CategoryLaws.idLeft")
    val xformRefs = scala.collection.mutable.ListBuffer[(String, String)]()
    
    spec.rules.foreach { rule =>
      // Only rule names like "Beta.forward" or "Xform.backward" reference xforms
      // Rule names like "CategoryLaws.idLeft" are just organizational namespaces
      // Strip type params from rule name too: "Map.forward[A,B]" -> "Map"
      val baseName = baseSortName(rule.name)
      if baseName.endsWith(".forward") then
        val xformName = baseName.dropRight(8)
        xformRefs += ((xformName, s"rule ${rule.name}"))
      else if baseName.endsWith(".backward") then
        val xformName = baseName.dropRight(9)
        xformRefs += ((xformName, s"rule ${rule.name}"))
    }
    
    // From patterns that call xforms
    spec.rules.foreach { rule =>
      rule.cases.foreach { c =>
        collectXformCalls(c.rhs).foreach(n => xformRefs += ((n, s"rule ${rule.name}")))
      }
    }
    
    spec.defs.foreach { d =>
      collectXformCalls(d.body).foreach(n => xformRefs += ((n, s"def ${d.name}")))
    }
    
    val undefined = xformRefs.toList.filter { case (n, _) => !definedXforms.contains(n) }
    val issues = undefined.map { case (n, loc) =>
      ValidationIssue(ValidationSeverity.Error, "undefined-xform", s"Undefined xform: $n", Some(loc))
    }
    
    ValidationResult(issues)
  
  private def collectXformCalls(pat: MetaPattern): List[String] =
    foldPattern(pat)(
      onVar = _ => Nil,
      onCon = (name, _) => {
        // Xform calls look like "Xform.forward(...)" or "Xform.backward(...)"
        if name.endsWith(".forward") then List(name.dropRight(8))
        else if name.endsWith(".backward") then List(name.dropRight(9))
        else Nil
      }
    )
  
  private def checkUndefinedRuleRefs(spec: LangSpec): ValidationResult =
    val definedRules = spec.rules.map(_.name).toSet
    
    // Check strategy references
    val ruleRefs = spec.strategies.values.flatMap(collectStrategyRuleRefs).toList
    
    val undefined = ruleRefs.filter(!definedRules.contains(_))
    val issues = undefined.distinct.map { n =>
      ValidationIssue(ValidationSeverity.Error, "undefined-rule", s"Undefined rule in strategy: $n")
    }
    
    ValidationResult(issues)
  
  private def collectStrategyRuleRefs(strat: RewriteStrategy): List[String] = strat match
    case RewriteStrategy.Apply(name) => List(name)
    case RewriteStrategy.Seq(a, b) => collectStrategyRuleRefs(a) ++ collectStrategyRuleRefs(b)
    case RewriteStrategy.Choice(a, b) => collectStrategyRuleRefs(a) ++ collectStrategyRuleRefs(b)
    case RewriteStrategy.Repeat(s) => collectStrategyRuleRefs(s)
    case RewriteStrategy.All(s) => collectStrategyRuleRefs(s)
    case RewriteStrategy.Unify(_, _) => Nil
    case RewriteStrategy.Id => Nil
  
  private def checkUndefinedDefRefs(spec: LangSpec): ValidationResult =
    val definedDefs = spec.defs.map(_.name).toSet
    
    // Collect def references from patterns
    val defRefs = scala.collection.mutable.ListBuffer[(String, String)]()
    
    spec.defs.foreach { d =>
      collectDefRefs(d.body).foreach(n => defRefs += ((n, s"def ${d.name}")))
    }
    
    // Filter to only check lowercase identifiers that might be def refs
    val potentialRefs = defRefs.toList.filter { case (name, _) =>
      name.headOption.exists(_.isLower) && !isBuiltinVar(name)
    }
    
    // This is tricky - we can't easily distinguish local pattern vars from def refs
    // So we just note when a lowercase identifier isn't a defined def
    // This should be a warning, not an error
    ValidationResult.empty // Skip for now - pattern vars make this complex
  
  private def collectDefRefs(pat: MetaPattern): List[String] =
    foldPattern(pat)(
      onVar = name => List(name),
      onCon = (_, _) => Nil
    )
  
  // ===========================================================================
  // Unused Item Checks (Warnings)
  // ===========================================================================
  
  private def checkUnusedSorts(spec: LangSpec): ValidationResult =
    val definedSorts = spec.sorts.map(_.name).toSet
    
    // Collect all sort usages
    val usedSorts = scala.collection.mutable.Set[String]()
    
    spec.constructors.foreach { con =>
      usedSorts += con.returnSort
      con.params.foreach { case (_, ty) =>
        usedSorts ++= collectSortRefs(ty)
      }
    }
    
    spec.xforms.foreach { x =>
      usedSorts += x.source
      usedSorts += x.target
    }
    
    spec.changes.foreach { c =>
      usedSorts += c.sort
    }
    
    spec.defs.foreach { d =>
      d.sort.foreach(usedSorts += _)
    }
    
    val unused = definedSorts -- usedSorts -- builtinSorts
    val issues = unused.map { s =>
      ValidationIssue(ValidationSeverity.Warning, "unused-sort", s"Unused sort: $s")
    }
    
    ValidationResult(issues.toList)
  
  private def checkUnusedConstructors(spec: LangSpec): ValidationResult =
    val definedCons = spec.constructors.map(_.name).toSet
    
    // Collect all constructor usages from patterns
    val usedCons = scala.collection.mutable.Set[String]()
    
    spec.rules.foreach { rule =>
      rule.cases.foreach { c =>
        usedCons ++= collectPatternConstructors(c.lhs)
        usedCons ++= collectPatternConstructors(c.rhs)
      }
    }
    
    spec.defs.foreach { d =>
      usedCons ++= collectPatternConstructors(d.body)
    }
    
    val unused = definedCons -- usedCons
    val issues = unused.map { c =>
      ValidationIssue(ValidationSeverity.Warning, "unused-constructor", s"Unused constructor: $c")
    }
    
    ValidationResult(issues.toList)
  
  private def checkUnusedXforms(spec: LangSpec): ValidationResult =
    val definedXforms = spec.xforms.map(_.name).toSet
    
    // Collect xform usages
    val usedXforms = scala.collection.mutable.Set[String]()
    
    // From rule names
    spec.rules.foreach { rule =>
      if rule.name.contains('.') then
        usedXforms += rule.name.takeWhile(_ != '.')
    }
    
    // From pattern xform calls
    spec.rules.foreach { rule =>
      rule.cases.foreach { c =>
        usedXforms ++= collectXformCalls(c.rhs)
      }
    }
    
    spec.defs.foreach { d =>
      usedXforms ++= collectXformCalls(d.body)
    }
    
    val unused = definedXforms -- usedXforms
    val issues = unused.map { x =>
      ValidationIssue(ValidationSeverity.Warning, "unused-xform", s"Unused xform: $x")
    }
    
    ValidationResult(issues.toList)
  
  private def checkUnusedRules(spec: LangSpec): ValidationResult =
    val definedRules = spec.rules.map(_.name).toSet
    
    // Collect rule usages from strategies
    val usedRules = spec.strategies.values.flatMap(collectStrategyRuleRefs).toSet
    
    val unused = definedRules -- usedRules
    val issues = unused.map { r =>
      ValidationIssue(ValidationSeverity.Warning, "unused-rule", s"Unused rule (not in any strategy): $r")
    }
    
    ValidationResult(issues.toList)
  
  // ===========================================================================
  // Pattern Variable Checks
  // ===========================================================================
  
  private def checkRulePatternVariables(spec: LangSpec): ValidationResult =
    val issues = scala.collection.mutable.ListBuffer[ValidationIssue]()
    
    spec.rules.foreach { rule =>
      rule.cases.foreach { c =>
        val lhsVars = collectPatternVars(c.lhs)
        val rhsVars = collectPatternVars(c.rhs)
        
        // Collect variables bound by where clauses (e.g., where s1 = Unify.forward(...))
        // In guards like RuleGuard("eq", PVar("s1"), expr), "s1" is being bound to expr
        val whereBoundVars = c.guards.flatMap { g =>
          if g.varName == "eq" then
            g.expr match
              case MetaPattern.PVar(v) if v.headOption.exists(_.isLower) && !v.contains('.') => 
                Some(v)
              case _ => None
          else None
        }.toSet
        
        // Collect variables used in guards (both for equality checks and bindings)
        val guardUsedVars = c.guards.flatMap { g =>
          collectPatternVars(g.expr) ++ collectPatternVars(g.expected)
        }.toSet
        
        // Check for RHS vars not bound in LHS or where clauses (excluding xform calls)
        val unbound = rhsVars.filter { v =>
          !lhsVars.contains(v) && 
          !whereBoundVars.contains(v) &&
          v.headOption.exists(_.isLower) && 
          !isBuiltinVar(v) &&
          !v.contains('.')  // Skip xform.forward etc
        }
        
        unbound.foreach { v =>
          issues += ValidationIssue(
            ValidationSeverity.Error, 
            "unbound-variable",
            s"Unbound variable '$v' in RHS (not bound in LHS)",
            Some(s"rule ${rule.name}")
          )
        }
        
        // Count occurrences of each var in LHS for non-linear pattern detection
        val lhsVarCounts = collectPatternVarCounts(c.lhs)
        val nonLinearVars = lhsVarCounts.filter(_._2 > 1).keySet  // vars appearing 2+ times = used for matching
        
        // Check for unused LHS vars (warning) - vars used in RHS, guards, or non-linear matching count as used
        val allUsedVars = rhsVars ++ guardUsedVars ++ nonLinearVars
        val unused = lhsVars.filter { v =>
          !allUsedVars.contains(v) && v != "_"
        }
        
        unused.foreach { v =>
          issues += ValidationIssue(
            ValidationSeverity.Warning,
            "unused-variable",
            s"Pattern variable '$v' bound but not used in RHS",
            Some(s"rule ${rule.name}")
          )
        }
      }
    }
    
    ValidationResult(issues.toList)
  
  /** Collect pattern variable occurrence counts */
  private def collectPatternVarCounts(pat: MetaPattern): Map[String, Int] =
    val counts = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    def go(p: MetaPattern): Unit = p match
      case MetaPattern.PVar(name) if name != "_" => counts(name) += 1
      case MetaPattern.PCon(_, args) => args.foreach(go)
      case MetaPattern.PApp(f, a) => go(f); go(a)
      case MetaPattern.PSubst(body, _, repl) => go(body); go(repl)
      case _ => ()
    go(pat)
    counts.toMap
  
  /** 
   * Collect free pattern variables, excluding lambda-bound variables.
   * Recognizes λa.body syntax which is parsed as PCon("lam", [PCon(a, []), _, body]).
   */
  private def collectPatternVars(pat: MetaPattern): Set[String] = 
    collectFreeVars(pat, Set.empty)
  
  private def collectFreeVars(pat: MetaPattern, bound: Set[String]): Set[String] = pat match
    case MetaPattern.PVar(name) => 
      if bound.contains(name) then Set.empty else Set(name)
    case MetaPattern.PCon("lam", List(MetaPattern.PCon(x, Nil), _, body)) =>
      // Lambda binds x in body
      collectFreeVars(body, bound + x)
    case MetaPattern.PCon(_, args) =>
      args.flatMap(a => collectFreeVars(a, bound)).toSet
    case MetaPattern.PApp(f, a) =>
      collectFreeVars(f, bound) ++ collectFreeVars(a, bound)
    case MetaPattern.PSubst(body, varName, repl) =>
      // The varName in substitution counts as a variable usage
      collectFreeVars(body, bound) ++ collectFreeVars(repl, bound) + varName
  
  // ===========================================================================
  // Constructor Arity Checks
  // ===========================================================================
  
  private def checkConstructorArity(spec: LangSpec): ValidationResult =
    // Build arity map, but mark variadic constructors specially
    val conArity = spec.constructors.map { c =>
      val isVariadic = c.params.exists { case (_, ty) => isVariadicType(ty) }
      c.name -> (if isVariadic then -1 else c.params.length)  // -1 means variadic
    }.toMap
    
    val issues = scala.collection.mutable.ListBuffer[ValidationIssue]()
    
    def checkPat(pat: MetaPattern, location: String): Unit = pat match
      case MetaPattern.PVar(_) => ()
      case MetaPattern.PCon(name, args) =>
        conArity.get(name).foreach { expected =>
          // Skip variadic constructors (expected == -1)
          if expected >= 0 && args.length != expected then
            issues += ValidationIssue(
              ValidationSeverity.Error,
              "arity-mismatch",
              s"Constructor $name expects $expected args, got ${args.length}",
              Some(location)
            )
        }
        args.foreach(checkPat(_, location))
      case MetaPattern.PApp(f, a) =>
        checkPat(f, location)
        checkPat(a, location)
      case MetaPattern.PSubst(body, _, repl) =>
        checkPat(body, location)
        checkPat(repl, location)
    
    spec.rules.foreach { rule =>
      rule.cases.foreach { c =>
        checkPat(c.lhs, s"rule ${rule.name} LHS")
        checkPat(c.rhs, s"rule ${rule.name} RHS")
      }
    }
    
    spec.defs.foreach { d =>
      checkPat(d.body, s"def ${d.name}")
    }
    
    ValidationResult(issues.toList)
  
  private def isVariadicType(ty: LangType): Boolean = ty match
    case LangType.ListOf(_) => true  // X* is parsed as ListOf
    case LangType.Product(_, r) => isVariadicType(r)
    case _ => false
  private def checkStrategyRefs(spec: LangSpec): ValidationResult =
    val definedRules = spec.rules.map(_.name).toSet
    val issues = scala.collection.mutable.ListBuffer[ValidationIssue]()
    
    def checkStrat(strat: RewriteStrategy, stratName: String): Unit = strat match
      case RewriteStrategy.Apply(ruleName) =>
        if !definedRules.contains(ruleName) then
          issues += ValidationIssue(
            ValidationSeverity.Error,
            "undefined-rule-in-strategy",
            s"Strategy references undefined rule: $ruleName",
            Some(s"strategy $stratName")
          )
      case RewriteStrategy.Seq(a, b) =>
        checkStrat(a, stratName)
        checkStrat(b, stratName)
      case RewriteStrategy.Choice(a, b) =>
        checkStrat(a, stratName)
        checkStrat(b, stratName)
      case RewriteStrategy.Repeat(s) =>
        checkStrat(s, stratName)
      case RewriteStrategy.All(s) =>
        checkStrat(s, stratName)
      case RewriteStrategy.Unify(_, _) => ()
      case RewriteStrategy.Id => ()
    
    spec.strategies.foreach { case (name, strat) =>
      checkStrat(strat, name)
    }
    
    ValidationResult(issues.toList)
  
  // ===========================================================================
  // Attribute Checks
  // ===========================================================================
  
  private def checkAttributeRefs(spec: LangSpec): ValidationResult =
    val definedSorts = spec.sorts.map(_.name).toSet ++ builtinSorts
    
    // Check that attribute types reference valid sorts
    val issues = spec.attributes.flatMap { attr =>
      collectSortRefs(attr.attrType).filterNot(definedSorts.contains).map { sortName =>
        ValidationIssue(ValidationSeverity.Error, "undefined",
          s"Attribute '${attr.name}' references undefined sort: $sortName")
      }
    }
    
    ValidationResult(issues)
  
  // ===========================================================================
  // Type String Parsing
  // ===========================================================================
  
  /** 
   * Parse a type string and extract all sort references.
   * Handles: SortName, Sort*, (A × B), (A → B), Sort[A,B], and nested combinations.
   */
  private def parseTypeString(typeStr: String): List[String] =
    // Remove whitespace for easier parsing
    val s = typeStr.trim
    
    // Handle list type: Sort*
    if s.endsWith("*") then
      parseTypeString(s.dropRight(1))
    // Handle parenthesized type: (A × B) or (A → B)
    else if s.startsWith("(") && s.endsWith(")") then
      val inner = s.drop(1).dropRight(1)
      // Find the operator (× or →) at the top level (not inside nested parens)
      findTopLevelOperator(inner) match
        case Some((left, right)) => parseTypeString(left) ++ parseTypeString(right)
        case None => parseTypeString(inner) // Just parentheses around a single type
    // Handle parameterized type: Sort[A, B]
    else if s.contains("[") && s.endsWith("]") then
      val bracketIdx = s.indexOf('[')
      val baseName = s.substring(0, bracketIdx)
      val argsStr = s.substring(bracketIdx + 1, s.length - 1)
      // Parse args (split by comma at top level)
      val args = splitTypeArgs(argsStr)
      baseName :: args.flatMap(parseTypeString)
    // Simple sort name (uppercase starting, alphanumeric)
    else if s.nonEmpty && s.head.isUpper && s.forall(c => c.isLetterOrDigit || c == '_') then
      List(s)
    else
      Nil // Not a valid sort reference (could be a variable or complex expr)
  
  /** Split type arguments by comma, respecting nested brackets */
  private def splitTypeArgs(s: String): List[String] =
    val result = scala.collection.mutable.ListBuffer[String]()
    var depth = 0
    var current = new StringBuilder
    for c <- s do
      c match
        case '[' => depth += 1; current += c
        case ']' => depth -= 1; current += c
        case ',' if depth == 0 => 
          result += current.toString.trim
          current = new StringBuilder
        case _ => current += c
    if current.nonEmpty then result += current.toString.trim
    result.toList
  
  /** Find top-level × or → operator, returning the left and right parts */
  private def findTopLevelOperator(s: String): Option[(String, String)] =
    var depth = 0
    var i = 0
    while i < s.length do
      s(i) match
        case '(' => depth += 1
        case ')' => depth -= 1
        case '×' | '→' if depth == 0 =>
          return Some((s.take(i).trim, s.drop(i + 1).trim))
        case _ =>
      i += 1
    None
  
  // ===========================================================================
  // Builtins
  // ===========================================================================
  
  private val builtinSorts = Set("String", "Int", "Bool", "Nat")
  private val builtinConstructors = Set("nil", "cons", "pair", "true", "false")
  private def isBuiltinVar(name: String): Boolean = 
    name == "_" || name == "nil" || name == "cons" || name == "true" || name == "false"

// =============================================================================
// Pretty Printer for Validation Results
// =============================================================================

object ValidationPrinter:
  def print(result: ValidationResult): String =
    val sb = new StringBuilder
    
    if result.errors.nonEmpty then
      sb.append(s"Errors (${result.errors.size}):\n")
      result.errors.foreach { issue =>
        val loc = issue.location.map(l => s" [$l]").getOrElse("")
        sb.append(s"  ✗ ${issue.message}$loc\n")
      }
    
    if result.warnings.nonEmpty then
      sb.append(s"Warnings (${result.warnings.size}):\n")
      result.warnings.foreach { issue =>
        val loc = issue.location.map(l => s" [$l]").getOrElse("")
        sb.append(s"  ⚠ ${issue.message}$loc\n")
      }
    
    if result.isValid && !result.hasWarnings then
      sb.append("✓ No issues found\n")
    
    sb.toString
