package lisp.compile


import lisp.ast.SExprAST._
import lisp.ast.LispExprAST._

object LispCompiler {

  def compile(parsed: SExpr): Expr = parsed match {
    case SSymbol(name) => Symbol(name)
    case SNumber(num) => Number(num)
    case SBool(bool) => Bool(bool)
    case SList(exprs) => exprs match {
      case SSymbol("quote") :: expr :: Nil => Quote(compile(expr))
      case SSymbol("define") :: SSymbol(name) :: expr :: Nil => Bind(Symbol(name), compile(expr))
      case SSymbol(fn) :: argexprs => FnApp(Symbol(fn), argexprs.map(compile))
      case other => throw new Exception(f"compile error - failed to compile subexpr $other")
    }
    case other => throw new Exception(f"compile error - failed to compile expr $other")
  }
}
