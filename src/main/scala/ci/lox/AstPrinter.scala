package ci.lox

class AstPrinter extends Expr.Visitor[String] {

  def print(expr: Expr): String = expr.accept(this)

  // functional solution
  def visit(expr: Expr): String = expr match {
    case Expr.Binary(left, operator, right) => parenthesize(operator.lexeme, left, right)
    case Expr.Grouping(expression) => parenthesize("group", expression)
    case Expr.Literal(value) => if (value == null) "nil" else value.toString
    case Expr.Unary(operator, right) => parenthesize(operator.lexeme, right)
  }

  private def parenthesize(name: String, exprs: Expr*) =
    exprs.map(expr => s" ${ expr.accept(this) }").mkString(s"($name", "", ")")
}

object AstPrinter {
  def main(args: Array[String]): Unit = {
    val expression = Expr.Binary(
      Expr.Unary(Token(MINUS, "-", null, 1),
        Expr.Literal(123)),
      Token(STAR, "*", null, 1),
      Expr.Grouping(Expr.Literal(45.67))
    )

    println(new AstPrinter().print(expression))
  }
}
