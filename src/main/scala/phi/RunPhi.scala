package phi

import java.nio.file.{Files, Paths, Path}

/**
 * CLI for running Phi language specifications.
 * 
 * Usage: 
 *   sbt "runMain phi.RunPhi examples/stlc-nat.phi fib5"  -- run definition
 *   sbt "runMain phi.RunPhi examples/scala.phi -"        -- validate only
 */
@main def RunPhi(specFile: String, defName: String): Unit =
  val validateOnly = defName == "-" || defName == ""
  println(s"=== Phi Language Runner ===")
  println(s"Loading: $specFile")
  
  // Parse spec with inheritance resolution
  parseSpecWithInheritance(specFile) match
    case Left(err) =>
      println(s"Parse error: $err")
    case Right(spec) =>
      println(s"Language: ${spec.name}")
      spec.parent.foreach(p => println(s"  Extends: $p"))
      println(s"  Sorts: ${spec.sorts.map(_.name).mkString(", ")}")
      println(s"  Constructors: ${spec.constructors.map(_.name).mkString(", ")}")
      println(s"  Rules: ${spec.rules.map(_.name).mkString(", ")}")
      
      // Debug: show first rule
      spec.rules.headOption.foreach { r =>
        println(s"  First rule ${r.name}:")
        r.cases.headOption.foreach { c =>
          println(s"    LHS: ${c.lhs}")
          println(s"    RHS: ${c.rhs}")
        }
      }
      
      println(s"  Definitions: ${spec.defs.map(_.name).mkString(", ")}")
      println(s"  Strategies: ${spec.strategies.keys.mkString(", ")}")
      println()
      
      // If no defName, just validate
      if validateOnly then
        println("✓ Validation successful")
        return
      
      // Run
      val interp = LangInterpreter(spec)
      
      println(s"Evaluating: $defName")
      try
        val term = interp.evalDef(defName)
        println(s"Initial: ${term.show}")
        println()
        println("Normalizing...")
        
        val start = System.currentTimeMillis()
        val result = interp.normalize(term)
        val elapsed = System.currentTimeMillis() - start
        val rate = if elapsed > 0 then result.steps * 1000 / elapsed else 0
        
        println(s"Result: ${result.value.show}")
        println(s"Time: ${elapsed}ms, Steps: ${result.steps}, Rate: $rate steps/s")
      catch
        case e: Exception =>
          println(s"Error: ${e.getMessage}")
          e.printStackTrace()

/** Parse a spec file and resolve inheritance and imports */
def parseSpecWithInheritance(specFile: String): Either[String, LangSpec] =
  val basePath = Option(Paths.get(specFile).getParent).getOrElse(Paths.get("."))
  
  def parseFile(file: String): Either[String, LangSpec] =
    val source = try
      Files.readString(Paths.get(file))
    catch
      case e: Exception =>
        return Left(s"Error reading $file: ${e.getMessage}")
    
    PhiParser.parse(source)
  
  def resolveImports(spec: LangSpec, visited: Set[String]): Either[String, LangSpec] =
    if spec.imports.isEmpty then
      Right(spec)
    else
      spec.imports.foldLeft(Right(spec): Either[String, LangSpec]) { (acc, imp) =>
        acc.flatMap { currentSpec =>
          val importPath = basePath.resolve(imp.path).toString
          if visited.contains(importPath) then
            Left(s"Circular import: ${visited.mkString(" -> ")} -> ${imp.path}")
          else
            for
              importedSpec <- parseFile(importPath)
              resolvedImport <- resolveImports(importedSpec, visited + importPath)
              merged = mergeImport(currentSpec, resolvedImport, imp)
            yield merged
        }
      }
  
  def resolveParent(spec: LangSpec, visited: Set[String]): Either[String, LangSpec] =
    spec.parent match
      case None => Right(spec)
      case Some(parentName) if visited.contains(parentName) =>
        Left(s"Circular inheritance: ${visited.mkString(" -> ")} -> $parentName")
      case Some(parentName) =>
        // Look for parent file: try λProlog.phi, prolog.phi, etc.
        val candidates = List(
          basePath.resolve(s"$parentName.phi"),
          basePath.resolve(s"${parentName.toLowerCase}.phi")
        )
        
        candidates.find(Files.exists(_)) match
          case Some(parentFile) =>
            for
              parentSpec <- parseFile(parentFile.toString)
              resolvedParent <- resolveParent(parentSpec, visited + parentName)
            yield mergeSpecs(resolvedParent, spec)
          case None =>
            Left(s"Parent language file not found: tried ${candidates.map(_.toString).mkString(", ")}")
  
  for
    spec <- parseFile(specFile)
    withImports <- resolveImports(spec, Set(specFile))
    resolved <- resolveParent(withImports, Set.empty)
  yield resolved

/** Merge imported spec into current spec based on import selectors */
def mergeImport(current: LangSpec, imported: LangSpec, imp: ImportDecl): LangSpec =
  val prefix = imp.alias.map(_ + ".").getOrElse("")
  
  // Filter imported items by selectors (empty = import all)
  def shouldImport(name: String): Boolean =
    imp.selectors.isEmpty || imp.selectors.contains(name)
  
  // Add prefix if alias is specified
  def prefixName(name: String): String = prefix + name
  
  val importedSorts = imported.sorts.filter(s => shouldImport(s.name))
    .map(s => if imp.alias.isDefined then s.copy(name = prefixName(s.name)) else s)
  val importedCtors = imported.constructors.filter(c => shouldImport(c.name))
    .map(c => if imp.alias.isDefined then c.copy(name = prefixName(c.name)) else c)
  val importedXforms = imported.xforms.filter(x => shouldImport(x.name))
    .map(x => if imp.alias.isDefined then x.copy(name = prefixName(x.name)) else x)
  val importedChanges = imported.changes.filter(c => shouldImport(c.name))
    .map(c => if imp.alias.isDefined then c.copy(name = prefixName(c.name)) else c)
  val importedRules = imported.rules.filter(r => shouldImport(r.name.split("\\.").head))
    .map(r => if imp.alias.isDefined then r.copy(name = prefixName(r.name)) else r)
  val importedDefs = imported.defs.filter(d => shouldImport(d.name))
    .map(d => if imp.alias.isDefined then d.copy(name = prefixName(d.name)) else d)
  
  current.copy(
    sorts = current.sorts ++ importedSorts,
    constructors = current.constructors ++ importedCtors,
    xforms = current.xforms ++ importedXforms,
    changes = current.changes ++ importedChanges,
    rules = current.rules ++ importedRules,
    defs = current.defs ++ importedDefs,
    strategies = current.strategies ++ imported.strategies.view.filterKeys(shouldImport).toMap
  )

/** Merge parent spec into child (child overrides parent) */
def mergeSpecs(parent: LangSpec, child: LangSpec): LangSpec =
  // Child's defs/rules/etc override parent's by name
  val childRuleNames = child.rules.map(_.name).toSet
  val childDefNames = child.defs.map(_.name).toSet
  val childConstructorNames = child.constructors.map(_.name).toSet
  val childSortNames = child.sorts.map(_.name).toSet
  val childAttrNames = child.attributes.map(_.name).toSet
  // For attr equations, we key by (attrName, patternCon, forChild)
  def eqKey(e: AttrEquation): (String, String, Option[String]) =
    val patCon = e.pattern match
      case MetaPattern.PCon(name, _) => name
      case _ => ""
    (e.attrName, patCon, e.forChild)
  val childEqKeys = child.attrEquations.map(eqKey).toSet
  
  LangSpec(
    name = child.name,
    sorts = parent.sorts.filterNot(s => childSortNames.contains(s.name)) ++ child.sorts,
    constructors = parent.constructors.filterNot(c => childConstructorNames.contains(c.name)) ++ child.constructors,
    xforms = parent.xforms ++ child.xforms,  // Merge xforms
    changes = parent.changes ++ child.changes,
    rules = parent.rules.filterNot(r => childRuleNames.contains(r.name)) ++ child.rules,
    defs = parent.defs.filterNot(d => childDefNames.contains(d.name)) ++ child.defs,
    strategies = parent.strategies ++ child.strategies,  // Child strategies override
    theorems = parent.theorems ++ child.theorems,
    attributes = parent.attributes.filterNot(a => childAttrNames.contains(a.name)) ++ child.attributes,
    attrEquations = parent.attrEquations.filterNot(e => childEqKeys.contains(eqKey(e))) ++ child.attrEquations,
    parent = child.parent  // Keep the original parent reference
  )
