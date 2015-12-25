package lisp.compile


import lisp.Env
import lisp.ast.Trees._

import scala.collection.immutable.ListMap

object TreeTransformers {

  //fn call ->
  //head should be anything that CAN evaluate to a Lambda/Native
  // i.e a lambda, symbol, a fn call that evalutes to lambda,symbol... any expr
  //todo: dont resolve to fncall in compile we need to eval head first

  def compile(parsed: SExpr): Expr = parsed match {
    case SSymbol(name) => Symbol(name)
    case SNumber(num) => Number(num)
    case SBool(bool) => Bool(bool)
    case SList(exprs) => exprs match {
      case SSymbol("quote") :: expr :: Nil => Quote(compile(expr))
      case SSymbol("define") :: SSymbol(name) :: expr :: Nil => Bind(Symbol(name), compile(expr))
      case SSymbol("lambda") :: SList(_params) :: body => { /*body is a seq of expressions*/
        _params match {
          case (params: List[SSymbol]) => Lambda(params.map(_.symbol),body.map(compile))
          case other => throw new Exception(f"compile error - expected params list but saw $other")
        }
      }
      case head :: tail => //function call
        FnCall(compile(head), tail.map(compile))
      case other => throw new Exception(f"compile error - failed to compile subexpr $other")
    }
    case other => throw new Exception(f"compile error - failed to compile expr $other")
  }

  def eval(expr: Expr, env: Env): Either[String,(Expr, Env)] = expr match {
    case symbol:Symbol => env.get(symbol).map(symval => Right(symval,env)).getOrElse(Left(s"symbol not found: $symbol"))
    case num: Number => Right(num,env)
    case bool: Bool => Right(bool,env)
    case UnitExpr => Right(UnitExpr,env)
    case Quote(e) => Right(e,env)
    case Bind(name,v) =>
      val tev = eval(v,env)
      tev match {
        case Left(e) => Left(e)
        case Right((ev,_)) => Right[String,(Expr,Env)](UnitExpr,env.updated(name,ev))
        //ignore env modification in bind v e.g (define x (define y 5)) only creates x->()
      }
    case Lambda(params,body) => Right(Lambda(params,body),env)
    case FnCall(head,args) =>
      val tevh = eval(head,env)
      tevh match {
        case Left(e) => Left(e)
        case Right((evaledh, _)) => evaledh match {
          case Lambda(params,body) =>
            if (params.length != args.length)
              Left(s"expected ${params.length} args but got $args in tree $expr")
            else {
              val evaledArgs = args.map(eval(_, env)).map(_.right.get._1) //todo error+ignores env update by args
              val localEnv: Env = ListMap(params.map(Symbol).zip(evaledArgs): _*) ++ env
              Right(body.foldLeft[(Expr, Env)]((UnitExpr, localEnv)) { case ((_, lastEnv), stmt) => eval(stmt, lastEnv).right.get })
            }
          case NativeMethod(method) =>
            val m = args.map(eval(_,env))
            println(s"evld args: $m")
            val evaledArgs = args.map(eval(_,env)).map(_.right.get._1)
            Right((method(evaledArgs), env))
          case other => Left(s"cant apply $head as a function")
        }
      }
  }


  type TreeMap = (Expr, Env) => (Expr, Env)

  case class TreeTransform(name: String, f: TreeMap) {
    def compose(next: TreeTransform) = TreeTransform(name + "/" + next.name, (e,s) => (f.tupled andThen next.f.tupled)((e,s)) )
    def andThen = compose _
    def apply(expr: Expr, env: Env) = f(expr,env)
  }

  object TreeTransform {
    def named(name: String)(f: TreeMap) = TreeTransform(name,f)
  }



  //eval expr
  def eval2(expr: Expr, env: Env): (Expr, Env) = {
    val transform =
      bindSymbols andThen
      substituteSymbols andThen
      fnCalls andThen
      unquote
    transform(expr,env)
  }

  def bindSymbols = TreeTransform.named("bindSymbols"){(expr,env) => expr match {
    case Bind(name,exp) => (UnitExpr, env.updated(name, eval2(exp,env)._1))
    case other => (other, env)
  }}

  def substituteSymbols = TreeTransform.named("substSymbols"){(expr, env) => expr match {
    case sym: Symbol => (env(sym), env)
    case other => (other,env)
  }}

  def fnCalls = TreeTransform.named("fncalls"){(expr, env) => expr match {
    case FnCall(head, args) =>
      val evaledHead = eval2(head,env)._1
      //evaled head can be a
      //lambda, native. not a symbol, since that would be substituted in substituteSymbols
      evaledHead match {
        case Lambda(params,body) =>
          if (params.length != args.length)
            throw new Exception(s"expected ${params.length} args but got $args in tree $expr")

          //todo: can evaled args have side effects? this will not update env
          val evaledArgs = args.map(eval2(_,env)._1)
          val localEnv: Env = ListMap(params.map(Symbol).zip(evaledArgs): _*) ++ env
          body.foldLeft[(Expr,Env)]((UnitExpr,env)){case ((_,lastEnv),stmt) => eval2(stmt, lastEnv) }
        case NativeMethod(method) =>
          val evaledArgs = args.map(eval2(_,env)._1)
          (method(evaledArgs), env)
        case other => (other,env)
      }
    case other => (other, env)

  }}

  def unquote = TreeTransform.named("unquote"){(expr,env) => expr match {
    case Quote(exp) => (exp, env)
    case other => (other, env)
  }}




}
