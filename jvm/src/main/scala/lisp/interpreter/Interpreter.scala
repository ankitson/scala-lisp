package lisp.interpreter

import lisp.Env
import lisp.ast.Trees._
import lisp.compile.TreeTransformers
import lisp.parse.Parsers

object Interpreter {

  def newEnv(): Env = {
    val nativeMethods = Map[Symbol, Expr](
      Symbol("+") -> NativeMethod({
        case nums: List[Number] => Number(nums.map(_.num).sum)
        case args => throw new Exception(f"native + only applies to numbers, but called on $args")
      }),
      Symbol("-") -> NativeMethod({
        case l: List[Number] if l.size == 2 => Number(l(0).num - l(1).num)
        case args => throw new Exception(f"unexpected arg type")
      }),
      Symbol("*") -> NativeMethod({
        case nums: List[Number] => Number(nums.map(_.num).product)
        case args => throw new Exception(f"unexpected arg type")
      }),
      Symbol("/") -> NativeMethod({
        case l: List[Number] if l.size == 2 => Number(l(0).num / l(1).num)
        case args => throw new Exception(f"unexpected arg type")
      })
    )
    val env = nativeMethods
    Env(env.toList:_*)
  }

  def apply(): (String => (Expr, Env)) = {
    var env = newEnv()
    (input: String) => {
      val parsed = Parsers.exprP.parse(input).get.value
      val compiled = TreeTransformers.compile(parsed)
      val (evaled,newenv) = TreeTransformers.eval2(compiled, env)
      env = newenv

      (evaled,env)
    }
  }

}
