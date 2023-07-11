import pudu.grammar._
import pudu.lexer.Lexer

enum BoundedToken:
  case Literal(x: Int)
  case Plus()
  case Times()

  case ERROR(str: String)
  case EOF()

object BoundedLexer extends Lexer[BoundedToken]:
  "[0-9]+" { s => BoundedToken.Literal(s.toInt) }
  "\\+" { BoundedToken.Plus() }
  "\\*" { BoundedToken.Times() }
  "[ \t]".ignore
  "." { BoundedToken.ERROR(_) }

  override def eof = BoundedToken.EOF()

/* BoundedExpr(size) matches all expressions (+ and *) with at most
 * 'size' numbers.
 * It contains 'O(size)' non terminals and productions. */
class BoundedExpr(size: Int) extends LanguageSpec[Int, BoundedToken]:
  val literal = Terminal[BoundedToken.Literal]
  val plus = Terminal[BoundedToken.Plus]("+")
  val times = Terminal[BoundedToken.Times]("*")

  val expr = NonTerminal[Int]("expr")

  /* indexed non terminals. fixedSizeExpr(i) generates expressions with
   * exactly 'i' numbers */
  val fixedSizeExpr = (1 to size).map(idx => idx -> NonTerminal[Int](s"exp$idx")).toMap

  override val eof = Terminal[BoundedToken.EOF]
  override val error = Terminal[BoundedToken.ERROR]
  override val start = expr
  override val precedence = Precedence().left(times).left(plus)

  (expr ::= fixedSizeExpr(1)) { identity }
  (fixedSizeExpr(1) ::= literal) { _.x }

  for idx <- (2 to size) do
    val nt = fixedSizeExpr(idx)
    val prev = fixedSizeExpr(idx - 1)
    (expr ::= nt) { identity }
    (nt ::= (literal, plus, prev)) { (l,_,r) => l.x + r }
    (nt ::= (literal, times, prev)) { (l,_,r) => l.x * r }
