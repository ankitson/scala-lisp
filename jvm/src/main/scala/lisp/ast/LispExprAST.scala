package lisp.ast

object LispExprAST {
  sealed trait Expr
  case class Symbol(name: String) extends Expr
  case class Number(num: Int) extends Expr
  case class Bool(bool: Boolean) extends Expr
  case class Quote(expr: Expr) extends Expr
  case class Bind(name: Symbol, value: Expr) extends Expr
  case class FnApp(name: Symbol, args: List[Expr]) extends Expr
}
