object Lisp extends App {

  // object parse {
    sealed trait SExp
    case class SList(list: List[SExp]) extends SExp
    case class SNum(val value: Int) extends SExp
    case class SSym(val value: String) extends SExp
    case class SBool(val value: Boolean) extends SExp

    import fastparse.all._

    private val wspc_chars = " \n\r\t".toSet[Char]
    private val num_chars = ('0' to '9').toSet[Char]
    private val lang_chars = "()#".toSet[Char]
    private val reserved_chars = wspc_chars ++ num_chars ++ lang_chars
    private val wspcP = P(CharPred(_.isWhitespace).rep)

    val numberP: Parser[SNum] = P(CharIn("+-").? ~ CharIn('0' to '9').rep(1)).!.map(d => SNum(d.toInt))
    val symbolP: Parser[SSym] = P(CharPred(!reserved_chars.contains(_)).rep(1).!).map(SSym)
    val boolP: Parser[SBool] = P("#" ~/ CharIn("tf").!).map(s => if (s == "t") true else false).map(SBool)
    val listP: Parser[SList] = P("(" ~/ (wspcP ~ sexprP).rep(1) ~ wspcP.? ~ ")").map(v => SList(v.toList))
    val sexprP: Parser[SExp] = P(numberP | boolP | symbolP | listP)
  // }

  // object evaluate {
    sealed trait Exp
    case class Sym(val value: String) extends Exp
    case class Num(val value: Int) extends Exp
    case class Bool(val value: Boolean) extends Exp
    case object Unit extends Exp
    case class Fail(msg: String) extends Exp

    sealed trait Procedure extends Exp
    case class NativeFunction(function: List[Exp] => Exp) extends Procedure

    case class Closure(body: Exp, freeVars: List[Sym]) extends Procedure


    case class Bind(varSym: Sym, value: Exp) extends Exp
    case class Quote(value: Exp) extends Exp

    case class ApplySym(procSym: Sym, args: List[Exp]) extends Exp
    case class ApplyF(proc: Procedure, args: List[Exp]) extends Exp

    //import parse._
    def genAST(sexp: SExp): Exp = sexp match {
      case SSym(v) => Sym(v)
      case SNum(v) => Num(v)
      case SBool(v) => Bool(v)
      case SList(lis) => lis match {
        case SSym("quote") :: sexp :: Nil => Quote(genAST(sexp))
        case SSym("define") :: SSym(name) :: sexp :: Nil => Bind(Sym(name), genAST(sexp))

        case SSym("closure") :: SList(declaredParams) :: sexp :: Nil => declaredParams match {
          case Nil => Closure(genAST(sexp), Nil)
          case pars: List[SSym] => Closure(genAST(sexp), pars.map(p => genAST(p).asInstanceOf[Sym]) )
          case other => Fail(f"expected params here but got $other")
        }

        case SSym(procName) :: sargs =>
          ApplySym(Sym(procName), sargs.map(genAST))
        case other => Fail(f"error compiling $other")
      }
    }

    def eval(exp: Exp, env: Map[Sym, Exp]): (Exp,Map[Sym,Exp]) = {
      def pureV(block: => Exp): (Exp, Map[Sym,Exp]) = (block,env)
      def unit(nenv: => Map[Sym,Exp]): (Exp, Map[Sym,Exp]) = (Unit,nenv)
      println(s"eval step $exp $env")
      exp match {
        case Sym(valu) => pureV { env.getOrElse(Sym(valu),Fail(f"Symbol $valu not found")) }
        case Num(valu) => pureV { Num(valu) }
        case Bool(valu) => pureV { Bool(valu) }
        case Unit => pureV { Unit }
        case Fail(msg) => pureV { Fail(msg) }
        case NativeFunction(fn) => pureV { NativeFunction(fn) }
        case Closure(body,freevars) => pureV { Closure(body,freevars) }
        case Bind(varSym, value) => unit { env.updated(varSym, value) }
        case Quote(exp) => pureV { exp }
        case ApplySym(procSym, args) =>
          val proc = eval(procSym, env)._1
          proc match {
            case p: Procedure => pureV { eval(ApplyF(p, args), env)._1 }
            case _ => pureV { Fail(s"$procSym is not a function") }
          }
        case ApplyF(proc,args) => println(s"apply $proc $args"); proc match {
          case Closure(body,freeVars) => {
            if (freeVars.size != args.size) pureV { Fail(s"expected ${freeVars.size} args") }
            else {
              //lookup each freevar in env and eval - call by value
              val evaledArgs = args.map(arg => eval(arg, env)._1)
              //subst evaledArg for arg in proc
              val newBindings = (freeVars zip evaledArgs)
              val closedEnv = env ++ newBindings
              pureV { eval(body,closedEnv)._1 }
            }
          }
          case NativeFunction(f) => pureV { println(s"calling native $f"); f(args.map(arg => eval(arg,env)._1)) }
        }
      }
    }
  // }

  // object lib {
    // import evaluate._

    //todo
    // import scala.reflect.runtime.universe._
    // def cast[T](exp: Exp)(implicit tag: TypeTag[T]): T =  exp match {
    def numToInt(exp: Exp): Int = exp match {
      case Num(n) => n
    }

    val natives: Map[Sym, Exp] = Map(
      Sym("+") -> NativeFunction(args => { println(s"adding $args"); Num(args.map(numToInt(_)).sum) }),
      Sym("-") -> NativeFunction(args => Num (numToInt(args(0)) - numToInt(args(1))) ),
      Sym("*") -> NativeFunction(args => Num ( args.map(numToInt(_)).foldLeft(1)(_ * _)) ),
      Sym("/") -> NativeFunction(args => Num (numToInt(args(0)) / numToInt(args(1))) ),
      Sym("=") -> NativeFunction(args => Bool (args(0) == args(1)) ),
      Sym("if") -> NativeFunction(args => {
        val test = args(0).asInstanceOf[Bool]
        val tbranch = args(1)
        val fbranch = args(2)
        if (test.value) tbranch else fbranch
      })
    )


  // }

  // object repl {
    import scala.io.StdIn._

    def loop(): Unit = {
      var env = natives
      while (true) {
        var input = readLine(">")
        var brackets = input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum
        while (brackets != 0) {
          input = input + readLine(">")
          brackets = input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum
        }
        val parsed = sexprP.parse(input).get.value
        println(s"parsed: $parsed")
        val compiled = genAST(parsed)
        println(s"compiled: $compiled")
        val (evaledv, newe) = eval(compiled, env)
        println(s"evaled: $evaledv")
        env = newe
      }

    }

    loop()



  // }
}
