package lisp

import evaluate._

object sugar {

  implicit class SugaredString(s: String) {
    def sym() = Sym(s)
  }

}
