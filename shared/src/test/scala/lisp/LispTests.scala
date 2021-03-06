package lisp

import parse._
import evaluate._
import lib._
import sugar._
import fastparse.core.Parsed
import utest._

object LispTests extends TestSuite {

  val _parse = (src: String) => sexpP.parse(src)
  val compile = (sexp: SExp) => genAST(sexp)
  val evaluate = (exp:Exp) => eval(exp,natives)._1

  def testCompile(source: String, SExp: SExp, expr: Exp) = {
    val Parsed.Success(res_SExp, _) = _parse(source).get
    assert (res_SExp == SExp)
    val res_expr = compile(SExp)
    assert (res_expr == expr)
    expr
  }

  def testEval[T](expr: Exp, value: T) = {
    val evaled = evaluate(expr)
    println(f"evaluted result \n $evaled")
    assert (evaled.asInstanceOf[T] == value)
  }

  def testRepl(statements: Seq[(String, Exp)], print:Boolean = false) = {
    var env = natives()
    for ((input, exp_val) <- statements) {
      if (print) println(s"running ${input}\nenv: $env")
      val parsed = _parse(input)
      assertMatch(parsed){case Parsed.Success(_,_) => }
      val sexp = parsed.get.value
      if (print) println(s"sexp: $sexp")
      val exp = compile(sexp)
      if (print) println(s"exp: $exp")
      val (evaled,nextEnv) = eval(exp, env)
      if (print) println(evaled)
      assert (evaled == exp_val)
      env = nextEnv
    }
  }

  val tests = TestSuite {
    'literals {
      testCompile("1", 1.snum, 1.lnum)
      testCompile("100", 100.snum, 100.lnum)
      testCompile("-096", -96.snum, -96.lnum)
      testCompile("#t", true.sbool, true.lbool)
      testCompile("#f", false.sbool, false.lbool)
    }

    'symbols {
      testCompile("xyz", "xyz".ssym, "xyz".lsym)
    }

    'binds {
      testCompile("(define x 5)", slist("define".ssym, "x".ssym, 5.snum), Bind("x".lsym,5.lnum))
      testRepl(Seq(
        ("(define x 5)", Unit),
        ("(+ x 1)", 6.lnum)
      ))

    }

    'fnapps {
      val src = "(+ 1 2 3 4)"
      val sexp = slist("+".ssym, 1.snum, 2.snum, 3.snum, 4.snum)
      val exp = Apply("+".lsym, List(1.lnum, 2.lnum, 3.lnum, 4.lnum)) //testCompile(src, sexp, exp)
      testEval(testCompile(src, sexp, exp), 10.lnum)
    }

    'lambdas {
      val ident = "(lambda (x) x)"
      val sexp = slist("lambda".ssym, slist("x".ssym), "x".ssym)
      val exp = Closure(freeVars=List("x".lsym), body="x".lsym)
      testCompile(ident, sexp, exp)

      'nested {
        val nest =  """(((lambda (outer) (lambda (inner) (+ outer inner))) 1) 2)"""
        val sexp =
          slist(slist(slist(
            "lambda".ssym,
            slist("outer".ssym),
            slist(
              "lambda".ssym,
              slist("inner".ssym),
              slist("+".ssym,"outer".ssym,"inner".ssym)
            )
          ),
          1.snum),
          2.snum)

        val exp =
          Apply(
            head = Apply(
              head = Closure(
                body = Closure(
                  body = Apply("+".lsym,List("outer".lsym, "inner".lsym)),
                  freeVars = List(Sym("inner"))
                ),
                freeVars = List(Sym("outer"))
              ),
              args = List(Num(1))
            ),
            args = List(Num(2))
          )
        testEval(testCompile(nest, sexp, exp),3.lnum)
      }

      'recursive {
        testRepl(Seq(
          ("(define add (lambda (a b) (if (= b 0) a (add (+ a 1) (- b 1)))))", Unit),
          ("(add 9 3)", 12.lnum)
        ))
      }
    }

    'if {
      'lazy {
        testRepl(Seq(
          ("(define f (lambda (x) (if (= x 0) (+ x 1) (/ 1 x) ) ) )", Unit),
          ("(f 0)", 1.lnum),
          ("(f 1)", 1.lnum)
        ))
      }
    }

  }

}
