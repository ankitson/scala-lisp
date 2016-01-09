package lisp

import parse._
import evaluate._
import lib._
import sugar._

import utest._

object LispTests extends TestSuite {

  val tests = TestSuite {
    import fastparse.core.Parsed

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

    def testProgram[T](source: String, SExp: SExp, expr: Exp, theval: Option[T] = None) = {
      testCompile(source, SExp, expr)
      for { v <- theval } testEval(expr, v)
    }

    'literals {
      testCompile("1", SNum(1), Num(1))
      testCompile("100", SNum(100), Num(100))
      testCompile("-096", SNum(-96), Num(-96))
      testCompile("#t", SBool(true), Bool(true))
      testCompile("#f", SBool(false), Bool(false))
    }

    'symbols {
      testCompile("x", SSym("x"), Sym("x"))
      testCompile("xyz", SSym("xyz"), Sym("xyz"))
    }

    'quotes {
      testCompile("(quote x)", SList(SSym("quote") :: SSym("x") :: Nil), Quote(Sym("x")))
      testCompile("(quote 5)", SList(SSym("quote") :: SNum(5) :: Nil), Quote(Num(5)))
      testCompile("(quote (quote x))",
        SList(
          SSym("quote") ::
          SList(
            SSym("quote") ::
            SSym("x") ::
            Nil) ::
          Nil
        ),
        Quote(Quote(Sym("x")))
      )
    }

    'binds {
      testCompile(
        "(define x 5)",
        SList(SSym("define") :: SSym("x") :: SNum(5) :: Nil),
        Bind(Sym("x"), Num(5))
      )
    }

    'fnapps {
      val listAdd = "(+ 1 2 3 4)"
      val SExp = SList(SSym("+") :: SNum(1) :: SNum(2) :: SNum(3) :: SNum(4) :: Nil)
      val expr = ApplySym(Sym("+"), Num(1) :: Num(2) :: Num(3) :: Num(4) :: Nil)
      val evaled = Num(10)
      testCompile(listAdd, SExp, expr)
      testEval(expr, evaled)
    }

    'lambdas {
      val ident = "(lambda (x) x)"
      val SExp = SList(SSym("lambda") :: SList(SSym("x") :: Nil) :: SSym("x") :: Nil)
      val expr = Closure(freeVars = List("x".sym), body = Sym("x"))
      testCompile(ident, SExp, expr)

      'nested {
        val nest = """(define nest
                     |  (lambda (outer)
                     |    (lambda (inner)
                     |      (+ outer inner)
                     |    )
                     |  )
                     |)""".stripMargin.trim
        val SExp =
          SList(List(
              SSym("define"),
              SSym("nest"),
              SList(List(
                  SSym("lambda"),
                  SList(List( SSym("outer") )),
                  SList(List(
                      SSym("lambda"),
                      SList(List( SSym("inner") )),
                      SList(List( SSym("+"), SSym("outer"), SSym("inner") ))
                    )
                  )
                )
              )
            )
          )

        val expr =
          Bind(
            Sym("nest"),
            Closure(
              body = Closure(
                body = ApplySym(Sym("+"), List(Sym("outer"), Sym("inner")) ),
                freeVars = List("inner".sym)

              ),
              freeVars = List("outer".sym)
            )
          )
        testCompile(nest,SExp,expr)

        val nest2 = """(
                      |  ((lambda (outer)
                      |    (lambda (inner) (+ outer inner)))
                      |     1)
                      |  2)""".stripMargin.trim
        val SExp2 =
          SList(List(
            SList(List(
              SList(List(
                SSym("lambda"),
                SList(List( SSym("outer") )),
                SList(List(
                  SSym("lambda"),
                  SList(List( SSym("inner") )),
                  SList(List( SSym("+"), SSym("outer"), SSym("inner") ))
                ))
              )),
              SNum(1)
            )),
            SNum(2))
          )
        val expr2 = ApplyProc(
            Closure(
              body = ApplyProc(
                Closure(
                  body = ApplySym(Sym("+"), List(Sym("outer"),Sym("inner"))),
                  freeVars = List("inner".sym)
                ),
                List(Num(1))
              ),
              freeVars = List("outer".sym)
            ),
            List(Num(2))
          )
        testCompile(nest2, SExp2, expr2)
        testEval(expr2,3)
      }

      // 'seqbody {
      //   val test =
      //     """
      //       |(lambda (x)
      //       |   (stmt1)
      //       |   (+ 5 5)
      //       |   x
      //       |)
      //     """.stripMargin.trim
      //   val SExp = SList(List(
      //     SSym("lambda"),
      //     SList(List(SSym("x"))),
      //     SList(List(SSym("stmt1"))),
      //     SList(List(SSym("+"), SNum(5), SNum(5))),
      //     SSym("x")
      //   ))
      //   val expr = Closure(
      //     freeVars = List("x"),
      //     body = List(
      //       ApplySym(Sym("stmt1"),Nil),
      //       ApplySym(Sym("+"), List(Num(5),Num(5))),
      //       Sym("x")
      //     )
      //   )
      //
      // }

    }

    'add {
      val add = "(+ 1 2)"
      val SExp = SList(List(
        SSym("+"),
        SNum(1),
        SNum(2)
      ))
      val expr = ApplySym(
        Sym("+"),
        List(Num(1),Num(2))
      )
      val evaled = Num(3)

      testCompile(add, SExp, expr)
      testEval(expr, evaled)
    }

    'programs {
      val add1 = "( (define add (lambda (a b) (if (= b 0) a (add (+ a 1) (- b 1))))) 9 3 )"
      testEval(compile(_parse(add1).get.value), 12)
    }

  }

}
