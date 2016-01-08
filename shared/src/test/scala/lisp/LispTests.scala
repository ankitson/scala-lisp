// package lisp
//
// import lisp.ast.Trees._
// import lisp.compile.TreeTransformers
// import lisp.interpreter.Interpreter
// import utest._
//
// object LispTests extends TestSuite {
//
//
//   val tests = TestSuite {
//     import fastparse.core.Parsed
//     import lisp.parse.Parsers
//
//     val parse = (src: String) => Parsers.exprP.parse(src)
//     val compile = (sexpr: SExpr) => TreeTransformers.compile(sexpr)
//     val eval = (expr:Expr) => TreeTransformers.eval(expr, Interpreter.newEnv)
//
//     def testCompile(source: String, sexpr: SExpr, expr: Expr) = {
//       println("-----------------------")
//       println(f"evaluating \n $source")
//       val Parsed.Success(res_sexpr, _) = parse(source).get
//       println(f"parsed ast \n $res_sexpr")
//       assert (res_sexpr == sexpr)
//       val res_expr = compile(sexpr)
//       println(f"compiled ast \n $expr")
//       assert (res_expr == expr)
//       expr
//     }
//
//     def testEval[T](expr: Expr, value: T) = {
//       val evld = eval(expr)
//       if (evld.isLeft) throw new Exception(s"eval error: ${evld.left.get}")
//       val (evaled,newenv) = eval(expr).right.get
//       println(f"evaluted result \n $evaled")
//       assert (evaled.asInstanceOf[T] == value)
//     }
//
//     def testProgram[T](source: String, sexpr: SExpr, expr: Expr, theval: Option[T] = None) = {
//       testCompile(source, sexpr, expr)
//       for { v <- theval } testEval(expr, v)
//     }
//
//     'literals {
//       testCompile("1", SNumber(1), Number(1))
//       testCompile("100", SNumber(100), Number(100))
//       testCompile("-096", SNumber(-96), Number(-96))
//       testCompile("#t", SBool(true), Bool(true))
//       testCompile("#f", SBool(false), Bool(false))
//     }
//
//     'symbols {
//       testCompile("x", SSymbol("x"), Symbol("x"))
//       testCompile("xyz", SSymbol("xyz"), Symbol("xyz"))
//     }
//
//     'quotes {
//       testCompile("(quote x)", SList(SSymbol("quote") :: SSymbol("x") :: Nil), Quote(Symbol("x")))
//       testCompile("(quote 5)", SList(SSymbol("quote") :: SNumber(5) :: Nil), Quote(Number(5)))
//       testCompile("(quote (quote x))",
//         SList(
//           SSymbol("quote") ::
//           SList(
//             SSymbol("quote") ::
//             SSymbol("x") ::
//             Nil) ::
//           Nil
//         ),
//         Quote(Quote(Symbol("x")))
//       )
//     }
//
//     'binds {
//       testCompile(
//         "(define x 5)",
//         SList(SSymbol("define") :: SSymbol("x") :: SNumber(5) :: Nil),
//         Bind(Symbol("x"), Number(5))
//       )
//     }
//
//     'fnapps {
//       val listAdd = "(+ 1 2 3 4)"
//       val sexpr = SList(SSymbol("+") :: SNumber(1) :: SNumber(2) :: SNumber(3) :: SNumber(4) :: Nil)
//       val expr = FnCall(Symbol("+"), Number(1) :: Number(2) :: Number(3) :: Number(4) :: Nil)
//       val evaled = Number(10)
//       testCompile(listAdd, sexpr, expr)
//       testEval(expr, evaled)
//     }
//
//     'lambdas {
//       val ident = "(lambda (x) x)"
//       val sexpr = SList(SSymbol("lambda") :: SList(SSymbol("x") :: Nil) :: SSymbol("x") :: Nil)
//       val expr = Lambda(List("x"),List(Symbol("x")))
//       testCompile(ident, sexpr, expr)
//
//       'nested {
//         val nest = """(define nest
//                      |  (lambda (outer)
//                      |    (lambda (inner) (+ outer inner) )
//                      |  )
//                      |)""".stripMargin.trim
//         val sexpr =
//           SList(List(
//               SSymbol("define"),
//               SSymbol("nest"),
//               SList(List(
//                   SSymbol("lambda"),
//                   SList(List( SSymbol("outer") )),
//                   SList(List(
//                       SSymbol("lambda"),
//                       SList(List( SSymbol("inner") )),
//                       SList(List( SSymbol("+"), SSymbol("outer"), SSymbol("inner") ))
//                     )
//                   )
//                 )
//               )
//             )
//           )
//
//         val expr =
//           Bind(
//             Symbol("nest"),
//             Lambda(
//               List("outer"),
//               List(Lambda(
//                 List("inner"),
//                 List(FnCall(Symbol("+"), List(Symbol("outer"), Symbol("inner")) ))
//               ))
//             )
//           )
//         testCompile(nest,sexpr,expr)
//
//         //todo: add lexical scope
//         val nest2 = """(
//                       |  ((lambda (outer)
//                       |    (lambda (inner) (+ outer inner)))
//                       |     1)
//                       |  2)""".stripMargin.trim
//         val sexpr2 =
//           SList(List(
//             SList(List(
//               SList(List(
//                 SSymbol("lambda"),
//                 SList(List( SSymbol("outer") )),
//                 SList(List(
//                   SSymbol("lambda"),
//                   SList(List( SSymbol("inner") )),
//                   SList(List( SSymbol("+"), SSymbol("outer"), SSymbol("inner") ))
//                 ))
//               )),
//               SNumber(1)
//             )),
//             SNumber(2))
//           )
//         val expr2 = FnCall(
//           FnCall(
//             Lambda(
//               List("outer"),
//               List(Lambda(
//                 List("inner"),
//                 List(FnCall(Symbol("+"), List(Symbol("outer"),Symbol("inner"))))
//               ))
//             ),
//             List(Number(1))
//           ),
//           List(Number(2))
//         )
//         testCompile(nest2, sexpr2, expr2)
//         testEval(expr2,3)
//       }
//
//       'seqbody {
//         val test =
//           """
//             |(lambda (x)
//             |   (stmt1)
//             |   (+ 5 5)
//             |   x
//             |)
//           """.stripMargin.trim
//         val sexpr = SList(List(
//           SSymbol("lambda"),
//           SList(List(SSymbol("x"))),
//           SList(List(SSymbol("stmt1"))),
//           SList(List(SSymbol("+"), SNumber(5), SNumber(5))),
//           SSymbol("x")
//         ))
//         val expr = Lambda(
//           params = List("x"),
//           body = List(
//             FnCall(Symbol("stmt1"),Nil),
//             FnCall(Symbol("+"), List(Number(5),Number(5))),
//             Symbol("x")
//           )
//         )
//
//       }
//
//     }
//
//     'add {
//       val add = "(+ 1 2)"
//       val sexpr = SList(List(
//         SSymbol("+"),
//         SNumber(1),
//         SNumber(2)
//       ))
//       val expr = FnCall(
//         Symbol("+"),
//         List(Number(1),Number(2))
//       )
//       val evaled = Number(3)
//
//       testCompile(add, sexpr, expr)
//       testEval(expr, evaled)
//     }
//   }
//
// }

//(define add (closure (a b) (if (= b 0) a (add (+ a 1) (- b 1)))))
