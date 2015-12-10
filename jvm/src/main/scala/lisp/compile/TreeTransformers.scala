package lisp.compile


import lisp.Symbols
import lisp.ast.Trees._

import scala.collection.immutable.ListMap

object TreeTransformers {

  //1. s-expr to expr
  def compile(parsed: SExpr): Expr = parsed match {
    case SSymbol(name) => Symbol(name)
    case SNumber(num) => Number(num)
    case SBool(bool) => Bool(bool)
    case SList(exprs) => exprs match {
      case SSymbol("quote") :: expr :: Nil => Quote(compile(expr))
      case SSymbol("define") :: SSymbol(name) :: expr :: Nil => Bind(Symbol(name), compile(expr))
      case SSymbol("lambda") :: SList(formals) :: SList(body) :: Nil => Lambda(body.map(compile), Symbols())
      case SSymbol(fn) :: argexprs => FnApp(Symbol(fn), argexprs.map(compile))
      case other => throw new Exception(f"compile error - failed to compile subexpr $other")
    }
    case other => throw new Exception(f"compile error - failed to compile expr $other")
  }

  val nativeMethods: Map[Symbol, List[Expr] => Expr] = Map(
    Symbol("+") ->
      ( (exprs: List[Expr]) => { Number(exprs.map(_.asInstanceOf[Number]).map(_.num).sum) } )
  )

  def bindSymbols(expr: Expr, env: Symbols): (Expr, Symbols) = expr match {
    case Bind(name, exp) => (UnitExpr, env.updated(name, total_eval(exp, env)._1))
    case other => (other, env)
  }

  def substituteSymbols(expr: Expr, env: Symbols): (Expr, Symbols) = expr match {
    case sym @ Symbol(_) => (env(sym), env)
    case other => (other, env)
  }

  def applyFunctions(expr: Expr, env: Symbols): (Expr, Symbols) = expr match {
    case FnApp(symbol, args) => {
      if (env.contains(symbol)) {
        val func = env(symbol).asInstanceOf[Lambda]
        if (func.param_env.keys.size != args.size) {
          throw new Exception(f"expected args ${func.param_env.keys} but got ${args}")
        }
        val local_env = new ListMap[Symbol,Expr]() ++ func.param_env.keys.zip(args)
        val scoped_lambda = func.copy(param_env = local_env)
        (scoped_lambda, env)
      }
      else if (nativeMethods.contains(symbol)) {
        val nativeMethod = nativeMethods(symbol)
        val evaled_args = args.map(total_eval(_, env)._1)
        (nativeMethod(evaled_args), env)
      }
      else {
        throw new Exception(f"undefined function $symbol")
      }
    }
    case other => (other, env)
  }

  def unquote(expr: Expr, env: Symbols): (Expr, Symbols) = expr match {
    case Quote(exp) => (exp, env)
    case other => (other, env)
  }

  def applyLambdas(expr: Expr, env: Symbols): (Expr, Symbols) = expr match {
    case Lambda(body, local_env) =>
      var last_val: Expr = UnitExpr
      var total_env = local_env ++ env //todo merge envs properly
      println(f"evaluating lambda with env: $total_env")
      for (expr <- body) {
        val (last_val1,new_env) = total_eval(expr, total_env)
        last_val = last_val1
        total_env = new_env
      }
      (last_val,total_env)
    case other => (other,env)
  }




  def total_eval(expr: Expr, env: Symbols): (Expr, Symbols) = {
    def printArgs[A,B](f: Function[(A,B), (A,B)], tag: String = "") =
      (args: (A,B)) => { println(f"[$tag] args: $args"); f(args) }

    val stages = List(
      bindSymbols _,
      substituteSymbols _,
      applyFunctions _,
      unquote _,
      applyLambdas _
    ).map(_.tupled).zipWithIndex.map{ case (fn, idx) => printArgs(fn, idx.toString)}


    val total = stages.reduceLeft(_ andThen _)
    total((expr,env))
  }

}
