package phi.phi

/**
 * TokenRender: Built-in token rendering for grammar interpretation
 * 
 * These are the special tokens recognized during grammar-based rendering.
 * Eventually this could be driven by a spec that defines token semantics.
 * 
 * Attribute Grammar Tokens:
 *   NL       - newline at current indent level
 *   INDENT   - increase indent for following tokens  
 *   DEDENT   - decrease indent for following tokens
 *   COMMENT  - start a comment (//)
 *   DOCSTART - start a doc comment (/**)
 *   DOCEND   - end a doc comment (*/)
 *   BLANKLINE - emit a blank line
 *   STRLIT   - string literal quote (")
 *   DQUOT    - double quote as string ("\\"")
 *   NLSTR    - newline as string literal ("\\n")
 *   COMMA    - comma in string (", ")
 *   SEMI     - semicolon in string ("; ")
 *   IDENT    - identifier token
 *   STRING   - string literal token
 *   INT      - integer literal token
 */
object TokenRender:

  /** Built-in token names that have special rendering semantics */
  val builtinTokens: Set[String] = Set(
    "NL", "INDENT", "DEDENT",
    "COMMENT", "DOCSTART", "DOCEND", "BLANKLINE",
    "STRLIT", "DQUOT", "NLSTR", "COMMA", "SEMI",
    "IDENT", "STRING", "INT"
  )

  /** Check if a token name is a built-in with special semantics */
  def isBuiltin(name: String): Boolean = builtinTokens.contains(name)

  /** Render context for attribute grammar (inherited indent level) */
  case class RenderCtx(indent: Int = 0):
    def indented: RenderCtx = copy(indent = indent + 1)
    def outdented: RenderCtx = copy(indent = math.max(0, indent - 1))
    def indentStr: String = "  " * indent

  /** Render a built-in token, returning (output, newContext) */
  def renderBuiltin(name: String, ctx: RenderCtx): (String, RenderCtx) = name match
    case "NL"        => ("\n" + ctx.indentStr, ctx)
    case "INDENT"    => ("", ctx.indented)
    case "DEDENT"    => ("", ctx.outdented)
    case "COMMENT"   => ("//", ctx)
    case "DOCSTART"  => ("/**", ctx)
    case "DOCEND"    => (" */", ctx)
    case "BLANKLINE" => ("\n", ctx)
    case "STRLIT"    => ("\"", ctx)
    case "DQUOT"     => ("\"\\\"\"", ctx)  // Renders as: "\""
    case "NLSTR"     => ("\"\\n\"", ctx)   // Renders as: "\n"
    case "COMMA"     => ("\", \"", ctx)
    case "SEMI"      => ("\"; \"", ctx)
    case _           => (s"/* unknown token: $name */", ctx)

  /** Punctuation characters that attach without space */
  val leftAttachPunctuation: Set[Char] = Set('(', ')', ',', ':', '.', '[', ']')

  /** Check if a string starts with punctuation that should attach left */
  def startsWithPunctuation(s: String): Boolean =
    s.headOption.exists(leftAttachPunctuation.contains)

  /** Check if a string ends with a character that next token should attach to */
  def endsWithAttach(s: String): Boolean =
    s.lastOption.exists(c => c == '(' || c == '.' || c == '[')

  /** Combine two rendered strings with appropriate spacing */
  def combine(acc: String, tok: String): String =
    if acc.isEmpty then tok
    else if tok.isEmpty then acc
    else if tok.startsWith("\n") then acc + tok
    else if acc.endsWith("\n") || acc.matches("(?s).*\\n[ ]*") then acc + tok
    else if startsWithPunctuation(tok) then acc + tok
    else if endsWithAttach(acc) then acc + tok
    else acc + " " + tok
