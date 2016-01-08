package lisp
import evaluate._

object lib {
  //todo
  // import scala.reflect.runtime.universe._
  // def cast[T](exp: Exp)(implicit tag: TypeTag[T]): T =  exp match
  def numToInt(exp: Exp): Int = exp match {
    case Num(n) => n
  }

  val natives: Map[Sym, Exp] = Map(
    Sym("+") -> NativeFunction(args => { Num(args.map(numToInt(_)).sum) }),
    Sym("-") -> NativeFunction(args => Num (numToInt(args(0)) - numToInt(args(1))) ),
    Sym("*") -> NativeFunction(args => Num ( args.map(numToInt(_)).foldLeft(1)(_ * _)) ),
    Sym("/") -> NativeFunction(args => Num (numToInt(args(0)) / numToInt(args(1))) ),
    Sym("=") -> NativeFunction(args => Bool (args(0) == args(1)) ),
    Sym("<") -> NativeFunction(args => Bool (numToInt(args(0)) < numToInt(args(1)) ) ),
    Sym(">") -> NativeFunction(args => Bool (numToInt(args(0)) > numToInt(args(1)) ) ),
    Sym("if") -> NativeFunction(args => {
      val test = args(0).asInstanceOf[Bool]
      val tbranch = args(1)
      val fbranch = args(2)
      if (test.value) tbranch else fbranch
    })
  )

}
