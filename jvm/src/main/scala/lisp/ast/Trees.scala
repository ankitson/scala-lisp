package lisp.ast

import scala.collection.immutable.ListMap

object Trees {

  sealed trait SExpr
  case class SList(exprs: List[SExpr]) extends SExpr
  case class SSymbol(symbol: String) extends SExpr
  case class SNumber(number: Int) extends SExpr
  case class SBool(bool: Boolean) extends SExpr

  trait Expr
  case class Symbol(name: String) extends Expr
  case class Number(num: Int) extends Expr
  case class Bool(bool: Boolean) extends Expr
  case class Quote(expr: Expr) extends Expr
  case class Bind(name: Symbol, value: Expr) extends Expr //this is 'define'
  case class FnApp(name: Symbol, args: List[Expr]) extends Expr
  case class Lambda(body: List[Expr], param_env: ListMap[Symbol, Expr]) extends Expr
  case object UnitExpr extends Expr

}
