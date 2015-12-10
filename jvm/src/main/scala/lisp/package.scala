import lisp.ast.Trees._

import scala.collection.immutable.ListMap

package object lisp {
  type Symbols = ListMap[Symbol, Expr]
  def Symbols(elems: (Symbol,Expr)*) = ListMap[Symbol,Expr](elems:_*)
}
