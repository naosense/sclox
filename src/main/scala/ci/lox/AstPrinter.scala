package ci.lox

class AstPrinter extends Expr.Visitor[String] {

  override def visitAssignExpr(expr: Expr.Assign): String = ???

  override def visitBinaryExpr(expr: Expr.Binary): String = parenthesize(expr.operator.lexeme, expr.left, expr.right)

  override def visitCallExpr(expr: Expr.Call): String = ???

  override def visitGetExpr(expr: Expr.Get): String = ???

  override def visitGroupingExpr(expr: Expr.Grouping): String = parenthesize("group", expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): String = if (expr.value == null) "nil" else expr.value.toString

  override def visitLogicalExpr(expr: Expr.Logical): String = ???

  override def visitSetExpr(expr: Expr.Set): String = ???

  override def visitSuperExpr(expr: Expr.Super): String = ???

  override def visitThisExpr(expr: Expr.This): String = ???

  override def visitUnaryExpr(expr: Expr.Unary): String = parenthesize(expr.operator.lexeme, expr.right)

  override def visitVariableExpr(expr: Expr.Variable): String = ???

  def print(expr: Expr): String = expr.accept(this)

  private def parenthesize(name: String, exprs: Expr*) = {
    exprs.map(expr => s" ${ expr.accept(this) }").mkString(s"($name", "", ")")
  }
}

object AstPrinter {
  def main(args: Array[String]): Unit = {
    val expression = Expr.Binary(
      Expr.Unary(
        Token(MINUS, "-", null, 1),
        Expr.Literal(123)
      ),
      Token(STAR, "*", null, 1),
      Expr.Grouping(Expr.Literal(45.67))
    )

    println(new AstPrinter().print(expression))
  }
}
