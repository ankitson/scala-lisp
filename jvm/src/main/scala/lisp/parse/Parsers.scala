package lisp.parse

import fastparse.all
import lisp.ast.Trees._

object Parsers {

  import fastparse.all._

  private val wspc_chars = " \n\r\t".toSet[Char]
  private val num_chars = ('0' to '9').toSet[Char]
  private val lang_chars = "()#-".toSet[Char]
  private val reserved_chars = wspc_chars ++ num_chars ++ lang_chars
  private val wspcP = P(CharPred(_.isWhitespace).rep)

  val symbolP: Parser[SSymbol] = P(CharPred(!reserved_chars.contains(_)).rep(1).!).map(SSymbol)
  val numberP: Parser[SNumber] = P(CharIn("+-").? ~ CharIn('0' to '9').rep(1)).!.map(d => SNumber(d.toInt))
  val boolP: Parser[SBool] = P("#" ~/ CharIn("tf").!).map(s => if (s == "t") true else false).map(SBool)
  val listP: Parser[SList] = P("(" ~/ (wspcP ~ exprP).rep(1) ~ ")").map(v => SList(v.toList))
  val exprP: Parser[SExpr] = P(symbolP | numberP | boolP | listP)
}
