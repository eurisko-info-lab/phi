package phi.meta

import phi.phi.*
import phi.meta.Val.*

/**
 * Minimal Meta Interpreter - applies transform rules to values
 */
class LangInterpreter(spec: LangSpec):
  
  /** Apply a named xform rule to a value */
  def applyXform(xformName: String, value: Val): Option[Val] =
    // Look for rules matching this xform name (e.g., "Greeting2Scala.hello")
    val matchingRules = spec.rules.filter(_.name.startsWith(xformName + "."))
    if matchingRules.nonEmpty then
      matchingRules.foldLeft[Option[Val]](None) { (acc, rule) =>
        acc.orElse(applyRule(rule, value))
      }
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
      acc.orElse(tryCase(c, value))
    }
  
  private def tryCase(c: RuleCase, value: Val): Option[Val] =
    // Try to match the pattern
    matchPattern(c.pattern, value).flatMap { bindings =>
      // Check guard if present
      val guardOk = c.guard.forall(checkGuard(_, bindings))
      if guardOk then
        // Substitute bindings into the body
        Some(substitute(c.body, bindings))
      else None
    }
  
  private def matchPattern(pattern: MetaPattern, value: Val): Option[Map[String, Val]] =
    pattern match
      case MetaPattern.PVar(name) =>
        Some(Map(name -> value))
      
      case MetaPattern.PCon(name, args) =>
        value match
          case VCon(vname, vargs) if vname == name && args.length == vargs.length =>
            args.zip(vargs).foldLeft[Option[Map[String, Val]]](Some(Map.empty)) {
              case (Some(bindings), (pat, v)) =>
                matchPattern(pat, v).map(bindings ++ _)
              case (None, _) => None
            }
          case _ => None
      
      case MetaPattern.PApp(func, arg) =>
        // Handle app patterns if needed
        None
      
      case MetaPattern.PSubst(_, _, _) =>
        None
  
  private def checkGuard(guard: RuleGuard, bindings: Map[String, Val]): Boolean =
    guard match
      case RuleGuard.IsConstructor(varName, conName) =>
        bindings.get(varName).exists {
          case VCon(n, _) => n == conName
          case _ => false
        }
      case RuleGuard.Equals(_, _) => true
  
  private def substitute(expr: MetaPattern, bindings: Map[String, Val]): Val =
    expr match
      case MetaPattern.PVar(name) =>
        bindings.getOrElse(name, VCon(name, Nil))
      
      case MetaPattern.PCon(name, args) =>
        // Check if this is a rule reference that should be recursively applied
        val transformedArgs = args.map(substitute(_, bindings))
        
        // Handle xform.forward references like Name2Scala.forward(name)
        if name.contains(".forward") then
          val xformName = name.replace(".forward", "")
          transformedArgs match
            case List(arg) =>
              applyXform(xformName, arg).getOrElse(VCon(name, transformedArgs))
            case _ => VCon(name, transformedArgs)
        else
          spec.xforms.find(_.name == name) match
            case Some(xform) if transformedArgs.length == 1 =>
              applyRules(xform.rules, transformedArgs.head).getOrElse(VCon(name, transformedArgs))
            case _ =>
              VCon(name, transformedArgs)
      
      case MetaPattern.PApp(func, arg) =>
        // Handle application patterns like XformName.forward(arg)
        val argVal = substitute(arg, bindings)
        func match
          case MetaPattern.PCon(name, Nil) if name.endsWith(".forward") =>
            val xformName = name.dropRight(8)
            applyXform(xformName, argVal).getOrElse(VCon(name, List(argVal)))
          case _ =>
            val funcVal = substitute(func, bindings)
            VCon("App", List(funcVal, argVal))
      
      case MetaPattern.PSubst(body, varName, replacement) =>
        val replacementVal = substitute(replacement, bindings)
        val newBindings = bindings + (varName -> replacementVal)
        substitute(body, newBindings)
