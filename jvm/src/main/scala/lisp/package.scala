import lisp.ast.Trees._

import scala.collection.immutable.ListMap

package object lisp {
  type Env = ListMap[Symbol, Expr]
  def Env(elems: (Symbol,Expr)*) = ListMap[Symbol,Expr](elems:_*)
}
