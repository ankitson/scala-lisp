package lisp
import evaluate._

object lib {
  //todo
  // import scala.reflect.runtime.universe._
  // def cast[T](exp: Exp)(implicit tag: TypeTag[T]): T =  exp match
  def numToInt(exp: Exp): Int = exp match {
    case Num(n) => n
  }

  def native(fn: List[Exp] => Exp) = NativeFunction(
    (args: List[Exp]) => {
      val errors = args.filter(_.isInstanceOf[EvalFail]).map(_.asInstanceOf[EvalFail])
      if (errors.size > 0) EvalFail(errors.foldLeft("")((s,x) => s + x.msg))
      else fn(args)
    }
  )

  def natives(): Map[Sym, Exp] = Map(
    Sym("+") -> native(args => Num(args.map(numToInt(_)).sum) ),
    Sym("-") -> native(args => Num (numToInt(args(0)) - numToInt(args(1))) ),
    Sym("*") -> native(args => Num ( args.map(numToInt(_)).foldLeft(1)(_ * _)) ),
    Sym("/") -> native(args => Num (numToInt(args(0)) / numToInt(args(1))) ),
    Sym("=") -> native(args => Bool (args(0) == args(1)) ),
    Sym("<") -> native(args => Bool (numToInt(args(0)) < numToInt(args(1)) ) ),
    Sym(">") -> native(args => Bool (numToInt(args(0)) > numToInt(args(1)) ) )
  )

}
