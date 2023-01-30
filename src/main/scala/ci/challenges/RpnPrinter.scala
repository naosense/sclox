package ci.challenges

import ci.lox.{Expr, MINUS, PLUS, STAR, Token}

class RpnPrinter extends Expr.Visitor[String] {

  def print(expr: Expr): String = expr.accept(this)

  // functional solution
  def visit(expr: Expr): String = expr match {
    case Expr.Binary(left, operator, right) => parenthesize(operator.lexeme, left, right)
    case Expr.Grouping(expression) => parenthesize("group", expression)
    case Expr.Literal(value) => if (value == null) "nil" else value.toString
    case Expr.Unary(operator, right) => parenthesize(operator.lexeme, right)
  }

  private def parenthesize(name: String, exprs: Expr*) =
    exprs.map(expr => s"${ expr.accept(this) }").mkString("", " ", s" $name")
}

object RpnPrinter {
  def main(args: Array[String]): Unit = {
    val expression = Expr.Binary(
      Expr.Binary(
        Expr.Literal(1),
        Token(PLUS, "+", null, 1),
        Expr.Literal(2)),
      Token(STAR, "*", null, 1),
      Expr.Binary(
        Expr.Literal(4),
        Token(MINUS, "-", null, 1),
        Expr.Literal(3))
    )

    println(new RpnPrinter().print(expression))
  }
}


