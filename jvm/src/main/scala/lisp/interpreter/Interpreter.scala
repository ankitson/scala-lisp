package lisp.interpreter

import lisp.Symbols
import lisp.ast.Trees._
import lisp.compile.TreeTransformers
import lisp.parse.Parsers

import scala.collection.immutable.ListMap
import scala.io._
object Interpreter {

  def repl_loop() = {

    var env = Symbols()

    while (true) {
      var input = StdIn.readLine(">")
      var brackets = input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum
      while (brackets != 0) {
        input = input + StdIn.readLine(">")
        brackets = input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum
      }

      val parsed = Parsers.exprP.parse(input).get.value
      val compiled = TreeTransformers.compile(parsed)
      val (evaled,newenv) = TreeTransformers.total_eval(compiled, env)

      env = newenv
      println(f"${show(evaled)} .......... ${showEnv(env)}")
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
    case FnApp(name, args) => f"$name(${args.mkString(",")})"
    case Lambda(body, param_env) => f"lambda $body $param_env"
    case UnitExpr => "()"
  }

  def main(args: Array[String]) = repl_loop()

}
