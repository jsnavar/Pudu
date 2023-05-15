package pudu.parser

import scala.util.Try

trait ErrorMsg:
  def msg: String
/** Syntax Error: Found unexpected token */
case class SyntaxError[Token](found: Token, expected: Iterable[String]) extends ErrorMsg:
  override def msg: String = 
    val foundStr = tokenToString(found)
    val expectedStr = if expected.isEmpty then "nothing (??)"
      else if expected.size == 1 then expected.head.toString
      else s"any of <${expected.mkString(','.toString)}>"
    s"Syntax error. Found $foundStr, expected $expectedStr"

/** Empty Input: This represents an input without any token */
object EmptyInputError extends ErrorMsg:
  override def msg: String = "Input is empty!"

/** Unexpected eof */
case class InputEndedUnexpectedly[Token](last: Token) extends ErrorMsg:
  override def msg: String =
    val lastStr = tokenToString(last)
    s"Input ended unexpectedly. Last token was $lastStr"

/** Generated whenever the lexer yields an error token */
case class LexError[Token](found: Token) extends ErrorMsg:
  override def msg: String =
    val positionString = tokenPosition(found).map(pos => s" at: $pos")
    "Lexical error" + positionString.getOrElse(".")

/** Gets the position of the token. The position is obtained by invoking
 *  a parameterless method named 'p' of the token, if present. */
def tokenPosition[Token](token: Token): Option[Any] =
  Try(token.getClass().getMethod("p").invoke(token)).toOption

/** String representation of a token, with position if defined */
def tokenToString[Token](token: Token): String =
  val name = token.getClass().getName().split('$').last
  val positionString = tokenPosition(token).map(pos => s" at: $pos")
  name + positionString.getOrElse("")
