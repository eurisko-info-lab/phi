package phi.meta

// Token stream for parsing
enum Lex:
  case Ident(name: String)
  case Keyword(kw: String)
  case Symbol(sym: String)
  case IntLit(n: Int)
  case StrLit(s: String)
  case Whitespace(ws: String)
  case EOF
  case HoleTok(label: Option[String])

  def render: String = this match
    case Ident(n) => n
    case Keyword(k) => k
    case Symbol(s) => s
    case IntLit(n) => n.toString
    case StrLit(s) => s"\"$s\""
    case Whitespace(ws) => ws
    case EOF => ""
    case HoleTok(_) => "?"

// Simple lexer
object Lexer:
  def tokenize(input: String): TokenStream =
    val tokens = scala.collection.mutable.ListBuffer[Lex]()
    var i = 0
    while i < input.length do
      val c = input(i)
      if c.isWhitespace then
        val start = i
        while i < input.length && input(i).isWhitespace do i += 1
        tokens += Lex.Whitespace(input.substring(start, i))
      else if c.isLetter || c == '_' then
        val start = i
        while i < input.length && (input(i).isLetterOrDigit || input(i) == '_') do i += 1
        val word = input.substring(start, i)
        tokens += Lex.Ident(word)
      else if c.isDigit then
        val start = i
        while i < input.length && input(i).isDigit do i += 1
        tokens += Lex.IntLit(input.substring(start, i).toInt)
      else if c == '"' then
        i += 1
        val start = i
        while i < input.length && input(i) != '"' do i += 1
        tokens += Lex.StrLit(input.substring(start, i))
        if i < input.length then i += 1
      else
        tokens += Lex.Symbol(c.toString)
        i += 1
    tokens += Lex.EOF
    TokenStream(tokens.toVector, 0)

case class TokenStream(tokens: Vector[Lex], pos: Int):
  def peek: Lex = if pos < tokens.length then tokens(pos) else Lex.EOF
  def advance: TokenStream = copy(pos = pos + 1)
  def skipWs: TokenStream =
    var p = pos
    while p < tokens.length && tokens(p).isInstanceOf[Lex.Whitespace] do p += 1
    copy(pos = p)
  def nonEmpty: Boolean = pos < tokens.length && peek != Lex.EOF

