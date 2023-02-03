package ci.challenges

import ci.lox.{Expr, MINUS, PLUS, STAR, Token}

class RpnPrinter extends Expr.Visitor[String] {

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

  private def parenthesize(name: String, exprs: Expr*) =
    exprs.map(expr => s"${ expr.accept(this) }").mkString("", " ", s" $name")
}

object RpnPrinter {
  def main(args: Array[String]): Unit = {
    val expression = Expr.Binary(
      Expr.Binary(
        Expr.Literal(1),
        Token(PLUS, "+", null, 1),
        Expr.Literal(2)
      ),
      Token(STAR, "*", null, 1),
      Expr.Binary(
        Expr.Literal(4),
        Token(MINUS, "-", null, 1),
        Expr.Literal(3)
      )
    )

    println(new RpnPrinter().print(expression))
  }
}


