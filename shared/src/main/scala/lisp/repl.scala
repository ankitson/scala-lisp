package lisp

import parse._
import evaluate._
import lib._
import scala.util.Try
import fastparse.all._

object repl extends App {
  import scala.io.StdIn._

  sealed trait Free[F[_],A]
  case class Return[F[_],A](a: A) extends Free[F,A]
  case class Bind[F[_],I,A](i: F[I], k: I => Free[F,A]) extends Free[F,A]



  def loop(env: Map[Sym,Exp]): Unit = {
    def step(line: String): (String,Map[Sym,Exp]) = {
      val parsed = sexpP.parse(line)
      parsed match {
        case Parsed.Success(v,i) =>
          val compiled = genAST(v)
          val (evaled,nextEnv) = eval(compiled,env)
          (parsed.toString + "\n" + compiled.toString + "\n" + evaled.toString, nextEnv)
        case fail: Parsed.Failure => ("parse error: " + fail.msg,env)
      }
    }

    def readExpr(): String = {
      var input = readLine(">")
      var brackets = input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum
      while (brackets != 0) {
        input = input + readLine(".")
        brackets = input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum
      }
      input
    }

    val input = readExpr()
    val (output,nextEnv) = step(input)
    println(output)
    loop(nextEnv)

    // while (true) {
    //   var input = readLine(">")
    //
    //   var brackets = input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum
    //   while (brackets != 0) {
    //     input = input + readLine(">")
    //     brackets = input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum
    //   }
    //
    //   //control commands
    //   if (input == "(debug)")
    //
    //
    //   val parsed = sexpP.parse(input).get.value
    //   //println(s"parsed: $parsed")
    //   val compiled = genAST(parsed)
    //   //println(s"compiled: $compiled")
    //   val (evaledv, newe) = eval(compiled, env)
    //   println(s"$evaledv")
    //   env = newe
    // }
  }


  loop(natives)
}
