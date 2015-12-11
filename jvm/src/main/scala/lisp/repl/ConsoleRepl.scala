package lisp.repl

import lisp._
import lisp.ast.Trees._
import lisp.interpreter.Interpreter

import scala.io.StdIn

object ConsoleRepl {

  def repl_loop() = {
    val interpreter = Interpreter()

    while (true) {
      var input = StdIn.readLine(">")
      var brackets = input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum
      while (brackets != 0) {
        input = input + StdIn.readLine(">")
        brackets = input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum
      }
      val (evaled, env) = interpreter(input)
      println(show(evaled))
    }
  }

  def showEnv(env: Symbols): String = {
    ( for ((symbol,expr) <- env) yield symbol.name + "=" + show(expr) ).mkString("[",",","]")
  }

  def show(expr: Expr) = expr match {
    case Symbol(name) => name
    case Number(n) => n
    case Bool(b) => b
    case Quote(exp) => exp
    case Bind(name, value) => f"[$name=$value]"
    case FnCall(name, args) => f"$name(${args.mkString(",")})"
    case Lambda(body, param_env) => f"lambda $body $param_env"
    case UnitExpr => "()"

    case NativeMethod(method) => f"native method $method"
  }

  def main(args: Array[String]) = repl_loop()

}
