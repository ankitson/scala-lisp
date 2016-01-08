package lisp

object parse {
  sealed trait SExp
  case class SList(list: List[SExp]) extends SExp
  case class SNum(val value: Int) extends SExp
  case class SSym(val value: String) extends SExp
  case class SBool(val value: Boolean) extends SExp

  import fastparse.all._

  private val wspc_chars = " \n\r\t".toSet[Char]
  private val num_chars = ('0' to '9').toSet[Char]
  private val lang_chars = "()#".toSet[Char]
  private val reserved_chars = wspc_chars ++ num_chars ++ lang_chars
  private val wspcP = P(CharPred(_.isWhitespace).rep)

  val numberP: Parser[SNum] = P(CharIn("+-").? ~ CharIn('0' to '9').rep(1)).!.map(d => SNum(d.toInt))
  val symbolP: Parser[SSym] = P(CharPred(!reserved_chars.contains(_)).rep(1).!).map(SSym)
  val boolP: Parser[SBool] = P("#" ~/ CharIn("tf").!).map(s => if (s == "t") true else false).map(SBool)
  val listP: Parser[SList] = P("(" ~/ (wspcP ~ sexpP).rep(0) ~ wspcP.? ~ ")").map(v => SList(v.toList))
  val sexpP: Parser[SExp] = P(numberP | boolP | symbolP | listP)
}
