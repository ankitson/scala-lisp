package lisp.compile


import lisp.Symbols
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
      case SSymbol("lambda") :: SList(_params) :: body :: Nil => {
        _params match {
          case (params: List[SSymbol]) =>
            Lambda(compile(body), params.map(_.symbol))
          case other => throw new Exception(f"compile error - expected params list but saw $other")
        }
      }
      case head :: tail => //function call
        val compiledHead = compile(head)
        compiledHead match {
          case symbol: Symbol => FnCall(Left(symbol), tail.map(compile))
          case lambda: Lambda => FnCall(Right(lambda), tail.map(compile))
          case _ => throw new Exception(f"invalid function call ${compiledHead :: tail}")
        }
      case other => throw new Exception(f"compile error - failed to compile subexpr $other")
    }
    case other => throw new Exception(f"compile error - failed to compile expr $other")
  }


  type TreeMap = (Expr, Symbols) => (Expr, Symbols)
  case class TreeTransform(name: String, f: TreeMap) {
    def compose(next: TreeTransform) = TreeTransform(name + "/" + next.name, (e,s) => (f.tupled andThen next.f.tupled)((e,s)) )
    def andThen = compose _
    def apply(expr: Expr, env: Symbols) = f(expr,env)
  }
  object TreeTransform {
    def named(name: String)(f: TreeMap) = TreeTransform(name,f)
  }

  //eval expr
  def eval(expr: Expr, env: Symbols): (Expr, Symbols) = {
    val transform =
      bindSymbols andThen
      substituteSymbols andThen
      resolveFnCallSymbols andThen
      resolveFnCallLambdas andThen
      unquote

    transform(expr,env)
  }

  def bindSymbols = TreeTransform.named("bindSymbols"){(expr,env) => expr match {
    case Bind(name,exp) => (UnitExpr, env.updated(name, eval(exp,env)._1))
    case other => (other, env)
  }}

  def substituteSymbols = TreeTransform.named("substSymbols"){(expr, env) => expr match {
    case sym: Symbol => (env(sym), env)
    case other => (other,env)
  }}


  //after this stage fncalls are right only
  //also applies natives. todo: split
  def resolveFnCallSymbols = TreeTransform.named("resolveFnCallSymbols"){(expr,env) => expr match {
    case FnCall(name, args) if name.isLeft =>
      val Left(sym) = name
      if (!env.contains(sym))
        throw new Exception(s"unable to find function $sym")
      env(sym) match { //todo: enforce via typelevel that env only has NativeMethod or Lambda
        case NativeMethod(method) =>
          val evaled_args = args.map(eval(_,env)._1)
          (method(evaled_args),env)
        case lambda: Lambda => (lambda,env)
        case other => throw new Exception(s"expected native/lambda in env($sym), got $other")
      }
    case other => (other,env)
  }}

  def resolveFnCallLambdas = TreeTransform.named("resolveFnCallLambdas"){(expr, env) => expr match {
    case FnCall(name,args) if name.isRight =>
      val Right(Lambda(body,params)) = name
      if (params.length != args.length)
        throw new Exception(s"expected ${params.length} args but got ${args}")

      //todo: can evaled args have side effects? this will not update env
      val evaledArgs = args.map(eval(_,env)._1)
      val localEnv: Symbols = ListMap(params.map(Symbol).zip(evaledArgs): _*) ++ env
      (eval(body,localEnv)._1,env)
    case other => (other,env)
  }}

  def unquote = TreeTransform.named("unquote"){(expr,env) => expr match {
    case Quote(exp) => (exp, env)
    case other => (other, env)
  }}




}
