package ci.lox

class Scanner(source: String) {
  private val keywords = Map(
    "and" -> AND,
    "class" -> CLASS,
    "else" -> ELSE,
    "false" -> FALSE,
    "for" -> FOR,
    "fun" -> FUN,
    "if" -> IF,
    "nil" -> NIL,
    "or" -> OR,
    "print" -> PRINT,
    "return" -> RETURN,
    "super" -> SUPER,
    "this" -> THIS,
    "true" -> TRUE,
    "var" -> VAR,
    "while" -> WHILE,
  )
  private var tokens = Vector[Token]()
  private var start = 0
  private var current = 0
  private var line = 1

  def scanTokens(): IndexedSeq[Token] = {
    while (!isAtEnd()) {
      start = current
      tokens = scanToken()
    }

    tokens = tokens :+ Token(EOF, "", null, line)
    tokens
  }

  private def scanToken() = {
    advance() match {
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_PAREN)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '!' => addToken(if (isMatch('=')) BANG_EQUAL else BANG)
      case '=' => addToken(if (isMatch('=')) EQUAL_EQUAL else EQUAL)
      case '<' => addToken(if (isMatch('=')) LESS_EQUAL else LESS)
      case '>' => addToken(if (isMatch('=')) GREATER_EQUAL else GREATER)
      case '/' =>
        if (isMatch('/')) {
          while (peek() != '\n' && !isAtEnd()) advance()
          tokens
        } else addToken(SLASH)
      case ' ' | '\r' | '\t' => tokens
      case '\n' => line += 1; tokens
      case '"' => string()
      case c =>
        if (isDigit(c))
          number()
        else if (isAlpha(c))
          identifier()
        else {
          Lox.error(line, "Unexpected character")
          tokens
        }
    }
  }

  private def identifier() = {
    while (isAlphaNumberic(peek())) advance()

    val str = source.substring(start, current)
    keywords.get(str) match {
      case Some(tokenType) => addToken(tokenType)
      case None => addToken(IDENTIFIER)
    }
  }

  private def isAlpha(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  private def isAlphaNumberic(c: Char) = isAlpha(c) || isDigit(c)

  private def number() = {
    while (isDigit(peek())) advance()

    if (peek() == '.' && isDigit(peekNext())) {
      advance()

      while (isDigit(peek())) advance()
    }

    addToken(NUMBER, source.substring(start, current).toDouble)
  }

  private def isDigit(c: Char) = c >= '0' && c <= '9'

  private def string(): Vector[Token] = {
    while (peek() != '"' && !isAtEnd()) {
      if (peek() == '\n') line += 1
      advance()
    }

    if (isAtEnd()) {
      Lox.error(line, "Unterminated string.")
      return tokens
    }

    advance()

    val str = source.substring(start + 1, current - 1)
    addToken(STRING, str)
  }

  private def isMatch(expected: Char): Boolean = {
    if (isAtEnd()) return false
    if (source.charAt(current) != expected) return false
    current += 1
    true
  }

  private def peek() = if (isAtEnd()) '\u0000' else source.charAt(current)

  private def peekNext() = if (current + 1 >= source.length) '\u0000' else source.charAt(current + 1)

  private def isAtEnd(): Boolean = current >= source.length()

  private def advance() = {
    val c = source.charAt(current)
    current += 1
    c
  }

  private def addToken(tokenType: TokenType): Vector[Token] = addToken(tokenType, null)

  private def addToken(tokenType: TokenType, literal: Any): Vector[Token] = {
    val text = source.substring(start, current)
    tokens :+ Token(tokenType, text, literal, line)
  }
}
