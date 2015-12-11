package lisp.compile


import lisp.Symbols
import lisp.ast.Trees._

object TreeTransformers {

  //compile s-expr to expr
  def compile(parsed: SExpr): Expr = parsed match {
    case SSymbol(name) => Symbol(name)
    case SNumber(num) => Number(num)
    case SBool(bool) => Bool(bool)
    case SList(exprs) => exprs match {
      case SSymbol("quote") :: expr :: Nil => Quote(compile(expr))
      case SSymbol("define") :: SSymbol(name) :: expr :: Nil => Bind(Symbol(name), compile(expr))
      case SSymbol("lambda") :: SList(formals) :: SList(body) :: Nil => {
        formals match {
          case (params: List[SSymbol]) =>
            Lambda(body.map(compile), params.map(_.symbol))
          case other => throw new Exception(f"compile error - expected params list but saw $other")
        }
      }
      case head :: tail => //function call
        compile(head) match {
          case symbol: Symbol => FnCall(Left(symbol), tail.map(compile))
          case lambda: Lambda => FnCall(Right(lambda), tail.map(compile))
          case _ => throw new Exception(f"invalid function call ${head :: tail}")
        }
      case other => throw new Exception(f"compile error - failed to compile subexpr $other")
    }
    case other => throw new Exception(f"compile error - failed to compile expr $other")
  }

  //eval expr
  def eval(expr: Expr, env: Symbols): (Expr, Symbols) = {
    val stages = List(
      bindSymbols _,
      substituteSymbols _,
      applyFunctions _,
      unquote _
    ).map(_.tupled)

    val total = stages.reduceLeft(_ andThen _)
    total((expr,env))
  }

  def bindSymbols(expr: Expr, env: Symbols): (Expr, Symbols) = expr match {
    case Bind(name, exp) => (UnitExpr, env.updated(name, eval(exp, env)._1))
    case other => (other, env)
  }

  def substituteSymbols(expr: Expr, env: Symbols): (Expr, Symbols) = expr match {
    case sym @ Symbol(_) => (env(sym), env)
    case other => (other, env)
  }

  def applyFunctions(expr: Expr, env: Symbols): (Expr, Symbols) = expr match {
    case FnCall(name, args) => name match {
      case Left(fn_name_sym) =>
        if (!env.contains(fn_name_sym)) {
          throw new Exception(f"undefined function $fn_name_sym")
        }

        val fn = env(fn_name_sym)

        fn match {
          case lambda @ Lambda(body, formals) =>
            applyFunctions(FnCall(Right(lambda), args), env)

          case NativeMethod(method) =>
            val evaled_args = args.map(eval(_, env)._1)
            (method(evaled_args),env)
          case _ => throw new Exception(f"symbol $fn_name_sym does not refer to a function")
        }
      case Right(Lambda(body,formals)) =>
        if (formals.length != args.length)
          throw new Exception(f"expected args $formals but got $args")

        val formal_syms = formals.map(Symbol)
        val evaled_args = args.map(eval(_, env)._1)
        val lambda_env: Symbols = env ++ Symbols(formal_syms.zip(evaled_args): _*)

        var last_val: Expr = UnitExpr
        var last_env = lambda_env

        for (expr <- body) {
          val (v,e) = eval(expr, last_env)
          last_val = v
          last_env = e
        }
        (last_val, env)
    }
    case other => (other, env)
  }


  def unquote(expr: Expr, env: Symbols): (Expr, Symbols) = expr match {
    case Quote(exp) => (exp, env)
    case other => (other, env)
  }



}
