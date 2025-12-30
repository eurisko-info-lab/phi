package phi.meta

import phi.phi.*
import phi.meta.Val.*
import phi.meta.gen.Show.show
import phi.meta.gen.Interp  // Use generated pattern matching and substitution

/**
 * Minimal Meta Interpreter - applies transform rules to values
 */
class LangInterpreter(spec: LangSpec):
  
  /** Apply a named xform rule to a value */
  def applyXform(xformName: String, value: Val): Option[Val] =
    // Look for rules matching this xform name (e.g., "Greeting2Scala.hello")
    val rules = spec.rules.filter(_.name.startsWith(xformName + "."))
    if rules.nonEmpty then
      applyRules(rules, value)
    else
      // Try to find a direct match
      spec.rules.find(_.name == xformName).flatMap { rule =>
        applyRule(rule, value)
      }
  
  /** Apply a list of rules, trying each until one matches */
  def applyRules(rules: List[Rule], value: Val): Option[Val] =
    rules.foldLeft[Option[Val]](None) { (acc, rule) =>
      acc.orElse(applyRule(rule, value))
    }
  
  /** Try to apply a single rule to a value */
  def applyRule(rule: Rule, value: Val): Option[Val] =
    rule.cases.foldLeft[Option[Val]](None) { (acc, c) =>
      acc.orElse(tryCase(rule.name, c, value))
    }
  
  private def tryCase(ruleName: String, c: RuleCase, value: Val): Option[Val] =
    // Try to match the pattern
    matchPattern(c.pattern, value).flatMap { bindings =>
      // Check guard if present
      val guardOk = c.guard.forall(checkGuard(_, bindings))
      if guardOk then
        // Substitute bindings into the body
        Some(substitute(c.body, bindings))
      else None
    }
  
  private def showVal(v: Val): String = v.show
  
  // Use generated matchPattern from gen/Interp
  private def matchPattern(pattern: MetaPattern, value: Val): Option[Map[String, Val]] =
    Interp.matchPattern(pattern, value)
  
  private def checkGuard(guard: RuleGuard, bindings: Map[String, Val]): Boolean =
    guard match
      case RuleGuard.IsConstructor(varName, conName) =>
        bindings.get(varName).exists {
          case VCon(n, _) => n == conName
          case _ => false
        }
      case RuleGuard.Equals(lhs, rhs) =>
        // Substitute both sides and compare
        val lhsVal = substitute(lhs, bindings)
        val rhsVal = substitute(rhs, bindings)
        lhsVal == rhsVal
  
  // Use generated substitute from gen/Interp, but extend with rule/xform handling
  private def substitute(expr: MetaPattern, bindings: Map[String, Val]): Val =
    expr match
      case MetaPattern.PCon(name, args) =>
        // Check if this is a rule reference that should be recursively applied
        val transformedArgs = args.map(substitute(_, bindings))
        
        // Handle StrConcat: meta-level string concatenation evaluated at substitution time
        // Only treat as StrConcat operation if there are arguments to concatenate
        if name == "StrConcat" && args.nonEmpty then
          val concatenated = transformedArgs.map(Interp.extractString).mkString
          VStr(concatenated)
        // Handle xform.forward references like Name2Scala.forward(name)
        else if name.contains(".forward") then
          val xformName = name.replace(".forward", "")
          transformedArgs match
            case List(arg) =>
              applyXform(xformName, arg).getOrElse(VCon(name, transformedArgs))
            case _ => VCon(name, transformedArgs)
        // Handle rule references like GenEval.members(rules) or GenEval.cases(rules)
        else if name.contains(".") then
          // This looks like a rule reference (e.g., GenEval.members)
          // Try to find and apply the rule
          val maybeRules = spec.rules.filter(_.name == name)
          if maybeRules.nonEmpty then
            transformedArgs match
              case List(arg) =>
                applyRules(maybeRules, arg).getOrElse(VCon(name, transformedArgs))
              case _ =>
                // Multiple args - wrap in tuple-like structure for matching
                val argTuple = transformedArgs match
                  case Nil => VCon("Unit", Nil)
                  case _ => VCon("Args", transformedArgs)
                applyRules(maybeRules, argTuple).getOrElse(VCon(name, transformedArgs))
          else
            VCon(name, transformedArgs)
        else
          spec.xforms.find(_.name == name) match
            case Some(xform) if transformedArgs.length == 1 =>
              applyRules(xform.rules, transformedArgs.head).getOrElse(VCon(name, transformedArgs))
            case _ =>
              VCon(name, transformedArgs)
      
      // Delegate other cases to generated Interp.substitute
      case _ => Interp.substitute(expr, bindings)
