package lisp
import parse._
import lib._

object evaluate {
  sealed trait Exp
  case class Sym(val value: String) extends Exp
  case class Num(val value: Int) extends Exp
  case class Bool(val value: Boolean) extends Exp
  case object Unit extends Exp

  case class Bind(varSym: Sym, value: Exp) extends Exp
  case class Quote(value: Exp) extends Exp

  sealed trait Procedure extends Exp
  case class NativeFunction(function: List[Exp] => Exp) extends Procedure
  case class Closure(body: Exp, freeVars: List[Sym]) extends Procedure
  case class IfElse(test: Exp, tBranch: Exp, fBranch: Exp) extends Procedure

  case class ApplySym(procSym: Sym, args: List[Exp]) extends Exp
  case class ApplyProc(proc: Procedure, args: List[Exp]) extends Exp
  case class Apply(head: Exp, args: List[Exp]) extends Exp

  case class SyntaxFail(msg: String) extends Exp
  case class EvalFail(msg: String) extends Exp

  def genAST(sexp: SExp): Exp = sexp match {
    case SSym(v) => Sym(v)
    case SNum(v) => Num(v)
    case SBool(v) => Bool(v)
    case SList(lis) => lis match {
      case SSym("quote") :: sexp :: Nil => Quote(genAST(sexp))
      case SSym("define") :: SSym(name) :: sexp :: Nil => Bind(Sym(name), genAST(sexp))

      case SSym("lambda") :: SList(declaredParams) :: sexp :: Nil => declaredParams match {
        case Nil => Closure(genAST(sexp), Nil)
        case pars: List[SSym] => Closure(genAST(sexp), pars.map(p => genAST(p).asInstanceOf[Sym]) )
        case other => SyntaxFail(f"expected params for lambda but got $other")
      }

      case SSym("if") :: test :: tBranch :: fBranch :: Nil =>
        IfElse(genAST(test), genAST(tBranch), genAST(fBranch))

      case SSym(procName) :: sargs =>
        Apply(Sym(procName), sargs.map(genAST))

      case head :: tail => genAST(head) match {
        case c: Closure => Apply(c,tail.map(genAST)) //ApplyProc(c, tail.map(genAST))
        case other => Apply(other,tail.map(genAST))
      }

      case other => SyntaxFail(f"error compiling $other")
    }
  }

  def showEnv(env: Map[Sym,Exp]): String = {
    env.map{case (k,v) => v match {
      case NativeFunction(_) => "" //s"'${k.value}'" + "->" + "(native)"
      case Closure(body,freeVars) => s"'${k.value}'" + "->" + "lambda " + freeVars.mkString("(",",",")")
      case other => s"'${k.value}'" + "->" + other
    }}.mkString("Env(", ",", ")")
  }

  def eval(exp: Exp, env: Map[Sym, Exp]): (Exp,Map[Sym,Exp]) = {
    def pureV(block: => Exp): (Exp, Map[Sym,Exp]) = (block,env)
    def unit(nenv: => Map[Sym,Exp]): (Exp, Map[Sym,Exp]) = (Unit,nenv)

    println(s"eval step:\n$exp\n${showEnv(env)}")
    exp match {
      case Sym(valu) => pureV { env.getOrElse(Sym(valu),EvalFail(f"Symbol $valu not found")) }
      case Num(valu) => pureV { Num(valu) }
      case Bool(valu) => pureV { Bool(valu) }
      case Unit => unit { env }

      case EvalFail(msg) => pureV { EvalFail(msg) }

      case NativeFunction(fn) => pureV { NativeFunction(fn) }
      case Closure(body,freevars) => pureV { Closure(body,freevars) }
      case Bind(varSym, value) => unit { env.updated(varSym, eval(value,env)._1) }
      case Quote(exp) => pureV { exp }

      case Apply(head,args) => head match {
        case Closure(body,freeVars) => {
          if (freeVars.size != args.size) pureV { EvalFail(s"expected ${freeVars.size} args") }
          else {
            val evaledArgs = args.map(arg => eval(arg, env)._1) //lookup each freevar in env and eval - call by value
            val newBindings = (freeVars zip evaledArgs) //subst evaledArg for arg in proc
            val closedEnv = env ++ newBindings
            pureV { eval(body,closedEnv)._1 }
          }
        }
        case NativeFunction(f) => pureV { f(args.map(arg => eval(arg,env)._1)) }
        case other => pureV { eval(Apply(eval(head,env)._1,args),env)._1 }

      }


      // case Apply(head,args) =>
      //   println(f"apply with args $args")
      //   val maybeProc = eval(head,env)._1
      //   maybeProc match {
      //     case p: Procedure => pureV { eval(ApplyProc(p, args),env)._1 }
      //     case _ => pureV { EvalFail(s"$maybeProc is not a procedure") }
      //   }
      // case ApplySym(procSym, args) =>
      //   val proc = eval(procSym, env)._1
      //   proc match {
      //     case p: Procedure => pureV { eval(ApplyProc(p, args), env)._1 }
      //     case _ => pureV { EvalFail(s"$procSym is not a function") }
      //   }
      // case ApplyProc(proc,args) => proc match {
      //   case Closure(body,freeVars) => {
      //     if (freeVars.size != args.size) pureV { EvalFail(s"expected ${freeVars.size} args") }
      //     else {
      //       val evaledArgs = args.map(arg => eval(arg, env)._1) //lookup each freevar in env and eval - call by value
      //       val newBindings = (freeVars zip evaledArgs) //subst evaledArg for arg in proc
      //       val closedEnv = env ++ newBindings
      //       pureV { eval(body,closedEnv)._1 }
      //     }
      //   }
      //   case NativeFunction(f) => pureV { f(args.map(arg => eval(arg,env)._1)) }
      // }
      case IfElse(_test,tBranch,fBranch) =>
        val test = eval(_test,env)._1
        test match {
          case Bool(b) => if (b) eval(tBranch, env) else eval(fBranch,env)
          case x => pureV { EvalFail(s"Expected a boolean when evaluating ${_test} but got $x") }
        }
    }
  }
}
