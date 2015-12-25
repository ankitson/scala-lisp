package lisp.repl

import lisp._
import lisp.ast.Trees._
import lisp.compile.TreeTransformers
import lisp.interpreter.Interpreter
import lisp.parse.Parsers
import pprint.PPrinter

import scala.io.StdIn

object ConsoleRepl {

  def repl_loop() = {
    //val interpreter = Interpreter()
    import pprint.Config.Colors._


    var env = Interpreter.newEnv()
    while (true) {
      var input = StdIn.readLine(">")
      var brackets = input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum
      while (brackets != 0) {
        input = input + StdIn.readLine(">")
        brackets = input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum
      }
      val parsed = Parsers.exprP.parse(input).get.value
      print("parse ok\t\t"); pprint.pprintln(parsed)
      val compiled = TreeTransformers.compile(parsed)
      print("compile ok\t\t"); pprint.pprintln(compiled)
      val (evaled,nextenv) = TreeTransformers.eval2(compiled, env)
      print("eval ok\t\t"); pprint.pprintln(evaled) //pprint.pprintln(s"eval OK\t\t ${show(evaled)}")
      env = nextenv
      print("new env\t\t"); pprint.pprintln(env)
      //pprint.pprintln(s"new env\t\t ${showEnv(env)}")
    }
  }


  def showEnv(env: Env): String = {
    ( for ((symbol,expr) <- env) yield symbol.name + "=" + show(expr) ).mkString("[",",","]")
  }

  def show(expr: Expr) = expr match {
    case Symbol(name) => name
    case Number(n) => n
    case Bool(b) => b
    case Quote(exp) => exp
    case Bind(name, value) => f"[$name=$value]"
    case FnCall(name, args) => f"$name(${args.mkString(",")})"
    case Lambda(body, param_env) => f"lambda(args=$param_env)(body=$body)"
    case UnitExpr => "()"

    case NativeMethod(method) => f"native($method)"
  }

  def main(args: Array[String]) = repl_loop()

}
