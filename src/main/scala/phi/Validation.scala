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
      checkStrategyRefs(spec)
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
    
    val issues = 
      sortDups.map(n => ValidationIssue(ValidationSeverity.Error, "duplicate", s"Duplicate sort: $n")) ++
      conDups.map(n => ValidationIssue(ValidationSeverity.Error, "duplicate", s"Duplicate constructor: $n")) ++
      xformDups.map(n => ValidationIssue(ValidationSeverity.Error, "duplicate", s"Duplicate xform: $n")) ++
      defDups.map(n => ValidationIssue(ValidationSeverity.Error, "duplicate", s"Duplicate definition: $n")) ++
      ruleDups.map(n => ValidationIssue(ValidationSeverity.Error, "duplicate", s"Duplicate rule: $n"))
    
    ValidationResult(issues)
  
  private def findDuplicates[A](items: List[A]): List[A] =
    items.groupBy(identity).filter(_._2.size > 1).keys.toList
  
  // ===========================================================================
  // Undefined Reference Checks
  // ===========================================================================
  
  private def checkUndefinedSortRefs(spec: LangSpec): ValidationResult =
    val definedSorts = spec.sorts.map(_.name).toSet ++ builtinSorts
    
    // Collect all sort references
    val sortRefs = scala.collection.mutable.ListBuffer[(String, String)]() // (sort, location)
    
    // From constructors (return type and param types)
    spec.constructors.foreach { con =>
      sortRefs += ((con.returnSort, s"constructor ${con.name}"))
      con.params.foreach { case (_, ty) =>
        collectSortRefs(ty).foreach(s => sortRefs += ((s, s"constructor ${con.name}")))
      }
    }
    
    // From xforms (source and target)
    spec.xforms.foreach { x =>
      sortRefs += ((x.source, s"xform ${x.name}"))
      sortRefs += ((x.target, s"xform ${x.name}"))
    }
    
    // From changes
    spec.changes.foreach { c =>
      sortRefs += ((c.sort, s"change ${c.name}"))
    }
    
    // From defs (if sort annotation present)
    spec.defs.foreach { d =>
      d.sort.foreach(s => sortRefs += ((s, s"def ${d.name}")))
    }
    
    val undefined = sortRefs.toList.filter { case (s, _) => !definedSorts.contains(s) }
    val issues = undefined.map { case (s, loc) =>
      ValidationIssue(ValidationSeverity.Error, "undefined-sort", s"Undefined sort: $s", Some(loc))
    }
    
    ValidationResult(issues)
  
  private def collectSortRefs(ty: LangType): List[String] = ty match
    case LangType.SortRef(name) => 
      // Skip complex expressions that look like types but aren't sort names
      if name.contains("(") || name.contains("×") || name.contains("→") then Nil
      else List(name)
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
    
    // Filter out pattern variables (lowercase) and xform calls (contain '.')
    val actualConRefs = conRefs.toList.filter { case (name, _) =>
      name.headOption.exists(_.isUpper) && !name.contains('.')
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
    val definedXforms = spec.xforms.map(_.name).toSet
    
    // Collect xform references from rule names (e.g., "Beta.forward")
    val xformRefs = scala.collection.mutable.ListBuffer[(String, String)]()
    
    spec.rules.foreach { rule =>
      // Rule names like "Beta.forward" reference xform "Beta"
      if rule.name.contains('.') then
        val xformName = rule.name.takeWhile(_ != '.')
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
        
        // Check for RHS vars not bound in LHS (excluding xform calls)
        val unbound = rhsVars.filter { v =>
          !lhsVars.contains(v) && 
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
        
        // Check for unused LHS vars (warning)
        val unused = lhsVars.filter { v =>
          !rhsVars.contains(v) && v != "_"
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
  
  private def collectPatternVars(pat: MetaPattern): Set[String] = 
    foldPattern(pat)(
      onVar = name => List(name),
      onCon = (_, _) => Nil
    ).toSet
  
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
      case RewriteStrategy.Id => ()
    
    spec.strategies.foreach { case (name, strat) =>
      checkStrat(strat, name)
    }
    
    ValidationResult(issues.toList)
  
  // ===========================================================================
  // Builtins
  // ===========================================================================
  
  private val builtinSorts = Set("String", "Int", "Bool")
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
