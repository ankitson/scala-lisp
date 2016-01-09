package lisp

import parse._
import evaluate._

object sugar {

  implicit def slist(sexps: SExp*) = SList(sexps.toList)

  class SugaredString(str: String) {
    def ssym() = SSym(str)
    def lsym() = Sym(str)
  }
  implicit def strToSugared(str:String): SugaredString = new SugaredString(str)

  class SugaredInt(i: Int) {
    def snum() = SNum(i)
    def lnum() = Num(i)
  }
  implicit def intToSugared(i: Int): SugaredInt = new SugaredInt(i)

  class SugaredBoolean(b: Boolean) {
    def sbool() = SBool(b)
    def lbool() = Bool(b)
  }
  implicit def boolToSugared(b: Boolean): SugaredBoolean = new SugaredBoolean(b)

}
