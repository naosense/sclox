package ci.lox

class Parser(tokens: IndexedSeq[Token]) {
  private var current = 0

  private def expression() = equality()

  private def equality() = {
    var expr = comparison()

    while (isMatch(BANG_EQUAL, EQUAL_EQUAL)) {
      val operator = previous()
      val right = comparison()
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def comparison(): Expr = {
    var expr = term()

    while (isMatch(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      val operator = previous()
      val right = term()
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def term(): Expr = {
    var expr = factor()

    while (isMatch(MINUS, PLUS)) {
      val operator = previous()
      val right = factor()
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def factor(): Expr = {
    var expr = unary()

    while (isMatch(SLASH, STAR)) {
      val operator = previous()
      val right = unary()
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def unary(): Expr = {
    if (isMatch(BANG, MINUS)) {
      val operator = previous()
      val right = unary()
      return Expr.Unary(operator, right)
    }

    primary()
  }

  private def primary(): Expr = {
    if (isMatch(FALSE)) return Expr.Literal(false)
    if (isMatch(TRUE)) return Expr.Literal(true)
    if (isMatch(NIL)) return Expr.Literal(null)
    if (isMatch(NUMBER, STRING)) return Expr.Literal(previous().literal)
    if (isMatch(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression")
      return Expr.Grouping(expr)
    }

    throw new IllegalStateException()
  }

  private def isMatch(types: TokenType*): Boolean = {
    for (t <- types) {
      if (check(t)) {
        advance()
        return true
      }
    }
    false
  }

  private def consume(tokenType: TokenType, message: String): Token = {
    if (check(tokenType)) return advance()
    throw error(peek(), message)
  }

  private def check(tpe: TokenType) = if (isAtEnd()) false else peek().tpe == tpe

  private def advance() = {
    if (!isAtEnd()) current += 1
    previous()
  }

  private def isAtEnd() = peek().tpe == EOF

  private def peek() = tokens(current)

  private def previous() = tokens(current - 1)

  private def error(token: Token, message: String) = {
    Lox.error(token, message)
    new ParserError()
  }

  class ParserError extends RuntimeException
}
