package lisp.ast


object SExprAST {
  sealed trait SExpr
  case class SList(exprs: List[SExpr]) extends SExpr
  case class SSymbol(symbol: String) extends SExpr
  case class SNumber(number: Int) extends SExpr
  case class SBool(bool: Boolean) extends SExpr
}




