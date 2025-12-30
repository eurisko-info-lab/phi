package phi.phi.gen

import scala.util.parsing.combinator.*
import phi.phi.*
import phi.meta.Val
import phi.meta.Val.*

/** Parser: Generated parser combinators from phi.phi grammar rules  */
object Parser extends RegexParsers:

  // ========== Token parsers ==========
  def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
  def string: Parser[String] = """"[^"]*"""".r ^^ { s => s.substring(1, s.length - 1) }

  // ========== pattern parser ==========
  def pattern: Parser[Val] = ident ^^ {
  case id => VCon("PVar", List(VStr(id))) } | ident ~ "(" ~ rep(pattern) ~ ")" ^^ {
  case id ~ _ ~ pattern ~ _ => VCon("PCon", List(VStr(id), VList(pattern))) } | pattern ~ pattern ^^ {
  case pattern ~ pattern1 => VCon("PApp", List(pattern, pattern1)) }

  // ========== decl parser ==========
  def decl: Parser[Val] = "sort" ~ ident ^^ {
  case _ ~ id => VCon("SortDecl", List(VStr(id))) } | "constructor" ~ ident ~ ":" ~ typeParser ^^ {
  case _ ~ id ~ _ ~ ty => VCon("ConstructorDecl", List(VStr(id), ty)) } | "grammar" ~ ident ~ "{" ~ rep(syntaxRule) ~ "}" ^^ {
  case _ ~ id ~ _ ~ syntaxRule ~ _ => VCon("GrammarDecl", List(VStr(id), VList(syntaxRule))) } | "xform" ~ ident ~ ":" ~ ident ~ "⇄" ~ ident ^^ {
  case _ ~ id ~ _ ~ id1 ~ _ ~ id2 => VCon("XformDecl", List(VStr(id), VStr(id1), VStr(id2))) } | "rule" ~ qualifiedName ~ "{" ~ ruleCase ~ "}" ^^ {
  case _ ~ qualifiedName ~ _ ~ ruleCase ~ _ => VCon("RuleDecl", List(qualifiedName, ruleCase)) }

  // ========== qualifiedName parser ==========
  def qualifiedName: Parser[Val] = ident ^^ {
  case id => VStr(id) }

  // ========== syntaxRule parser ==========
  def syntaxRule: Parser[Val] = rep1(syntaxToken) ~ "=>" ~ syntaxArg ^^ {
  case syntaxToken ~ _ ~ syntaxArg => VCon("SynRule", List(VList(syntaxToken), syntaxArg)) }

  // ========== spec parser ==========
  def spec: Parser[Val] = "language" ~ ident ~ "{" ~ rep(decl) ~ "}" ^^ {
  case _ ~ id ~ _ ~ decl ~ _ => VCon("LangSpec", List(VStr(id), VList(decl))) }

  // ========== syntaxArg parser ==========
  def syntaxArg: Parser[Val] = ident ^^ {
  case id => VCon("ArgRef", List(VStr(id))) } | string ^^ {
  case str => VCon("ArgStrLit", List(VStr(str))) } | ident ~ "(" ~ rep(syntaxArg) ~ ")" ^^ {
  case id ~ _ ~ syntaxArg ~ _ => VCon("ArgCon", List(VStr(id), VList(syntaxArg))) } | "?" ^^ {
  case _ => VCon("ArgHole", Nil) }

  // ========== syntaxToken parser ==========
  def syntaxToken: Parser[Val] = string ^^ {
  case str => VCon("Literal", List(VStr(str))) } | ident ^^ {
  case id => VCon("NonTerm", List(VStr(id), VStr(""))) } | ident ~ "*" ^^ {
  case id ~ _ => VCon("NonTerm", List(VStr(id), VStr("*"))) } | ident ~ "+" ^^ {
  case id ~ _ => VCon("NonTerm", List(VStr(id), VStr("+"))) } | ident ~ "?" ^^ {
  case id ~ _ => VCon("NonTerm", List(VStr(id), VStr("?"))) }

  // ========== ruleCase parser ==========
  def ruleCase: Parser[Val] = pattern ~ "↦" ~ pattern ^^ {
  case pattern ~ _ ~ pattern1 => VCon("Case", List(pattern, pattern1)) } | pattern ~ "|" ~ guard ~ "↦" ~ pattern ^^ {
  case pattern ~ _ ~ guard ~ _ ~ pattern1 => VCon("CaseGuarded", List(pattern, guard, pattern1)) }

  // ========== guard parser ==========
  def guard: Parser[Val] = ident ~ "is" ~ ident ^^ {
  case id ~ _ ~ id1 => VCon("IsConstructor", List(VStr(id), VStr(id1))) } | pattern ~ "==" ~ pattern ^^ {
  case pattern ~ _ ~ pattern1 => VCon("Equals", List(pattern, pattern1)) }

  // ========== type parser ==========
  def typeParser: Parser[Val] = ident ^^ {
  case id => VCon("SortRef", List(VStr(id))) } | typeParser ~ "→" ~ typeParser ^^ {
  case ty ~ _ ~ ty1 => VCon("Arrow", List(ty, ty1)) } | "List" ~ "[" ~ typeParser ~ "]" ^^ {
  case _ ~ _ ~ ty ~ _ => VCon("ListOf", List(ty)) } | typeParser ~ "×" ~ typeParser ^^ {
  case ty ~ _ ~ ty1 => VCon("Product", List(ty, ty1)) }
