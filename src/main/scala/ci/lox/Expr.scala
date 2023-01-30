package ci.lox

sealed abstract class Expr {

  import ci.lox.Expr.Visitor

  def accept[R](visitor: Visitor[R]): R
}

object Expr {
  case class Assign(name: Token, value: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitAssignExpr(this)
  }
  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitBinaryExpr(this)
  }
  case class Call(callee: Expr, paren: Token, arguments: List[Expr]) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitCallExpr(this)
  }
  case class Get(obj: Expr, name: Token) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitGetExpr(this)
  }
  case class Grouping(expression: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitGroupingExpr(this)
  }
  case class Literal(value: Any) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitLiteralExpr(this)
  }
  case class Logical(left: Expr, operator: Token, right: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitLogicalExpr(this)
  }
  case class Set(obj: Expr, name: Token, value: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitSetExpr(this)
  }
  case class Super(keyword: Token, method: Token) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitSuperExpr(this)
  }
  case class This(keyword: Token) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitThisExpr(this)
  }
  case class Unary(operator: Token, right: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitUnaryExpr(this)
  }
  case class Variable(name: Token) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitVariableExpr(this)
  }

  trait Visitor[R] {
    def visitAssignExpr(expr: Assign): R = visit(expr)

    def visitBinaryExpr(expr: Binary): R = visit(expr)

    def visitCallExpr(expr: Call): R = visit(expr)

    def visitGetExpr(expr: Get): R = visit(expr)

    def visitGroupingExpr(expr: Grouping): R = visit(expr)

    def visitLiteralExpr(expr: Literal): R = visit(expr)

    def visitLogicalExpr(expr: Logical): R = visit(expr)

    def visitSetExpr(expr: Set): R = visit(expr)

    def visitSuperExpr(expr: Super): R = visit(expr)

    def visitThisExpr(expr: This): R = visit(expr)

    def visitUnaryExpr(expr: Unary): R = visit(expr)

    def visitVariableExpr(expr: Variable): R = visit(expr)

    def visit(expr: Expr): R
  }
}

