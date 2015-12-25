package lisp

import lisp.ast.Trees._
import lisp.compile.TreeTransformers
import lisp.interpreter.Interpreter
import utest._

object LispTests extends TestSuite {


  val tests = TestSuite {
    import fastparse.core.Parsed
    import lisp.parse.Parsers

    val parse = (src: String) => Parsers.exprP.parse(src)
    val compile = (sexpr: SExpr) => TreeTransformers.compile(sexpr)
    val eval = (expr:Expr) => TreeTransformers.eval(expr, Interpreter.newEnv)

    def testCompile(source: String, sexpr: SExpr, expr: Expr) = {
      println("-----------------------")
      println(f"evaluating \n $source")
      val Parsed.Success(res_sexpr, _) = parse(source).get
      println(f"parsed ast \n $res_sexpr")
      assert (res_sexpr == sexpr)
      val res_expr = compile(sexpr)
      println(f"compiled ast \n $expr")
      assert (res_expr == expr)
      expr
    }

    def testEval[T](expr: Expr, value: T) = {
      val (evaled,newenv) = eval(expr)
      println(f"evaluted result \n $evaled")
      assert (evaled.asInstanceOf[T] == value)
    }

    def testProgram[T](source: String, sexpr: SExpr, expr: Expr, theval: Option[T] = None) = {
      testCompile(source, sexpr, expr)
      for { v <- theval } testEval(expr, v)
    }

    'literals {
      testCompile("1", SNumber(1), Number(1))
      testCompile("100", SNumber(100), Number(100))
      testCompile("-096", SNumber(-96), Number(-96))
      testCompile("#t", SBool(true), Bool(true))
      testCompile("#f", SBool(false), Bool(false))
    }

    'symbols {
      testCompile("x", SSymbol("x"), Symbol("x"))
      testCompile("xyz", SSymbol("xyz"), Symbol("xyz"))
    }

    'quotes {
      testCompile("(quote x)", SList(SSymbol("quote") :: SSymbol("x") :: Nil), Quote(Symbol("x")))
      testCompile("(quote 5)", SList(SSymbol("quote") :: SNumber(5) :: Nil), Quote(Number(5)))
      testCompile("(quote (quote x))",
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
      testCompile(
        "(define x 5)",
        SList(SSymbol("define") :: SSymbol("x") :: SNumber(5) :: Nil),
        Bind(Symbol("x"), Number(5))
      )
    }

    'fnapps {
      val listAdd = "(+ 1 2 3 4)"
      val sexpr = SList(SSymbol("+") :: SNumber(1) :: SNumber(2) :: SNumber(3) :: SNumber(4) :: Nil)
      val expr = FnCall(Symbol("+"), Number(1) :: Number(2) :: Number(3) :: Number(4) :: Nil)
      val evaled = Number(10)
      testCompile(listAdd, sexpr, expr)
      testEval(expr, evaled)
    }

    'lambdas {
      val ident = "(lambda (x) x)"
      val sexpr = SList(SSymbol("lambda") :: SList(SSymbol("x") :: Nil) :: SSymbol("x") :: Nil)
      val expr = Lambda(List("x"),Symbol("x"))
      testCompile(ident, sexpr, expr)

      'nested {
        val nest = """(define nest
                     |  (lambda (x)
                     |    (lambda (y) (+ x y) )
                     |  )
                     |)""".stripMargin.trim
        val sexpr =
          SList(List(
              SSymbol("define"),
              SSymbol("nest"),
              SList(List(
                  SSymbol("lambda"),
                  SList(List( SSymbol("x") )),
                  SList(List(
                      SSymbol("lambda"),
                      SList(List( SSymbol("y") )),
                      SList(List( SSymbol("+"), SSymbol("x"), SSymbol("y") ))
                    )
                  )
                )
              )
            )
          )

        val expr =
          Bind(
            Symbol("nest"),
            Lambda(
              List("x"),
              Lambda(
                List("y"),
                FnCall(Symbol("+"), List(Symbol("x"), Symbol("y")) )
              )
            )
          )
        testCompile(nest,sexpr,expr)

        //todo: add lexical scope
        val nest2 = """(
                      |  ((lambda (x)
                      |    (lambda (y) (+ x y) )
                      |  )
                      |  1)
                      |  2
                      |)"""
        val expr2 = compile(parse(nest2).get.value)
        testEval(expr2, 3)
      }
    }

    'add {
      val add = "(+ 1 2)"
      val sexpr = SList(List(
        SSymbol("+"),
        SNumber(1),
        SNumber(2)
      ))
      val expr = FnCall(
        Symbol("+"),
        List(Number(1),Number(2))
      )
      val evaled = Number(3)

      testCompile(add, sexpr, expr)
      testEval(expr, evaled)
    }



//    'if {
//      val ifsrc = "(if (greater 2 3) (cons 1 nil) 2)"
//      val sexpr = SList(List(
//        SSymbol("if"),
//        SList(List(
//          SSymbol("greater"),
//          SNumber(2),
//          SNumber(3)
//        )),
//        SList(List(
//          SSymbol("cons"),
//          SNumber(1),
//          SSymbol("nil")
//        )),
//        SNumber(2)
//      ))
//      val expr = FnApp(
//        Symbol("if"),
//        List(
//          FnApp(Symbol("greater"), Number(2) :: Number(3) :: Nil),
//          FnApp(Symbol("cons"), Number(1) :: Symbol("nil") :: Nil),
//          Number(2)
//        )
//      )
//      val evaled = Number(2)
//
//      testCompile(ifsrc, sexpr, expr)
//      testEval(expr, evaled)
//
//    }
  }

}
