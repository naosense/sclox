package ci.lox

class Interpreter extends Expr.Visitor[Any] {
  def interpret(expression: Expr) = {
    try {
      val value = evaluate(expression)
      println(stringfy(value))
    } catch {
      case ex: RuntimeError => Lox.runtimeError(ex)
    }
  }

  override def visitAssignExpr(expr: Expr.Assign): Any = ???

  override def visitBinaryExpr(expr: Expr.Binary): Any = {
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    expr.operator.tpe match {
      case PLUS =>
        if (left.isInstanceOf[Double] && right.isInstanceOf[Double]) {
          return left.asInstanceOf[Double] + right.asInstanceOf[Double]
        }
        if (left.isInstanceOf[String] && right.isInstanceOf[String]) {
          return left.asInstanceOf[String] + right.asInstanceOf[String]
        }

        throw RuntimeError(
          expr.operator,
          "Operands must be two numbers or two strings."
        )
      case MINUS =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] - right.asInstanceOf[Double]
      case SLASH =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] / right.asInstanceOf[Double]
      case STAR =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] * right.asInstanceOf[Double]
      case GREATER =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] > right.asInstanceOf[Double]
      case GREATER_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] >= right.asInstanceOf[Double]
      case LESS =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] < right.asInstanceOf[Double]
      case LESS_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] <= right.asInstanceOf[Double]
      case BANG_EQUAL => !isEqual(left, right)
      case EQUAL_EQUAL => isEqual(left, right)
    }

    // unreachable
    null
  }

  override def visitCallExpr(expr: Expr.Call): Any = ???

  override def visitGetExpr(expr: Expr.Get): Any = ???

  override def visitGroupingExpr(expr: Expr.Grouping): Any = evaluate(expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): Any = expr.value

  override def visitLogicalExpr(expr: Expr.Logical): Any = ???

  override def visitSetExpr(expr: Expr.Set): Any = ???

  override def visitSuperExpr(expr: Expr.Super): Any = ???

  override def visitThisExpr(expr: Expr.This): Any = ???

  override def visitUnaryExpr(expr: Expr.Unary): Any = {
    val right = evaluate(expr.right)

    expr.operator.tpe match {
      case MINUS =>
        checkNumberOperand(expr.operator, right)
        -right.asInstanceOf[Double]
    }

    // unreachable
    null
  }

  private def checkNumberOperand(operator: Token, operand: Any): Unit = {
    if (operand.isInstanceOf[Double]) return
    throw RuntimeError(operator, "Operand must be a number.")
  }

  private def checkNumberOperands(operator: Token, left: Any, right: Any): Unit = {
    if (left.isInstanceOf[Double] && right.isInstanceOf[Double]) return
    throw RuntimeError(operator, "Operand must be a numbers.")
  }

  private def isTruthy(any: Any): Boolean = {
    if (any == null) return false
    any match {
      case bool: Boolean => return bool
      case _ => true
    }
  }

  private def isEqual(a: Any, b: Any): Boolean = {
    if (a == null && b == null) return true
    if (a == null) return false

    a.equals(b)
  }

  private def stringfy(any: Any): String = {
    if (any == null) return "nil"

    if (any.isInstanceOf[Double]) {
      var text = any.toString
      if (text.endsWith(".0")) {
        text = text.substring(0, text.length - 2)
      }
      return text
    }

    any.toString
  }

  override def visitVariableExpr(expr: Expr.Variable): Any = ???

  private def evaluate(expr: Expr) = expr.accept(this)
}

case class RuntimeError(token: Token, message: String) extends RuntimeException