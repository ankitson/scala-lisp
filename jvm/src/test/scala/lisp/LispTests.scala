package lisp

import lisp.ast.LispExprAST._
import lisp.ast.SExprAST._
import lisp.compile.LispCompiler
import utest._

object LispTests extends TestSuite {


  val tests = TestSuite {
    import fastparse.core.Result
    import lisp.parse.Parsers

    val parser = Parsers.exprP
    val compiler = LispCompiler

    def testEval(source: String, exp_pexpr: SExpr, exp_expr: Expr) = {
      println("-----------------------")
      println(f"evaluating \n $source")
      val Result.Success(pexpr, _) = parser.parse(source)
      println(f"parsed ast \n $pexpr")
      assert (pexpr == exp_pexpr)
      val expr = compiler.compile(pexpr)
      println(f"compiled ast \n $expr")
      assert (expr == exp_expr)
    }

    'literals {
      testEval("1", SNumber(1), Number(1))
      testEval("100", SNumber(100), Number(100))
      testEval("-096", SNumber(-96), Number(-96))
      testEval("#t", SBool(true), Bool(true))
      testEval("#f", SBool(false), Bool(false))
    }

    'symbols {
      testEval("x", SSymbol("x"), Symbol("x"))
      testEval("xyz", SSymbol("xyz"), Symbol("xyz"))
    }

    'quotes {
      testEval("(quote x)", SList(SSymbol("quote") :: SSymbol("x") :: Nil), Quote(Symbol("x")))
      testEval("(quote 5)", SList(SSymbol("quote") :: SNumber(5) :: Nil), Quote(Number(5)))
      testEval("(quote (quote x))",
        SList(
          SSymbol("quote") ::
          SList(
            SSymbol("quote") ::
            SSymbol("x") ::
            Nil) ::
          Nil
        ),
        Quote(Quote(Symbol("x")))
      )
    }

    'binds {
      testEval(
        "(define x 5)",
        SList(SSymbol("define") :: SSymbol("x") :: SNumber(5) :: Nil),
        Bind(Symbol("x"), Number(5))
      )
    }

    'fnapps {
      testEval(
        "(+ 1 2 3 4)",
        SList(SSymbol("+") :: SNumber(1) :: SNumber(2) :: SNumber(3) :: SNumber(4) :: Nil),
        FnApp(Symbol("+"), Number(1) :: Number(2) :: Number(3) :: Number(4) :: Nil)
      )
    }

    'add {
      val add = "(plus 1 2)"
      val pexpr = SList(List(
        SSymbol("plus"),
        SNumber(1),
        SNumber(2)
      ))
      val expr = FnApp(
        Symbol("plus"),
        List(Number(1),Number(2))
      )

      testEval(add, pexpr, expr)
    }

    'if {
      val ifsrc = "(if (greater 2 3) (cons 1 nil) 2)"
      val pexpr = SList(List(
        SSymbol("if"),
        SList(List(
          SSymbol("greater"),
          SNumber(2),
          SNumber(3)
        )),
        SList(List(
          SSymbol("cons"),
          SNumber(1),
          SSymbol("nil")
        )),
        SNumber(2)
      ))
      val expr = FnApp(
        Symbol("if"),
        List(
          FnApp(Symbol("greater"), Number(2) :: Number(3) :: Nil),
          FnApp(Symbol("cons"), Number(1) :: Symbol("nil") :: Nil),
          Number(2)
        )
      )

      testEval(ifsrc, pexpr, expr)
    }
  }

}
