package pudu.parser

import scala.util.Try

trait ErrorMsg:
  def msg: String

def expectedStr(expected: Iterable[String]) =
  if expected.isEmpty then "nothing (??)"
  else if expected.size == 1 then expected.head.toString
  else
    val alternatives = expected.mkString(",")
    s"any of <$alternatives>"

/** Syntax Error: Found unexpected token */
case class SyntaxError[Token](found: Token, terminalName: String, expected: Iterable[String]) extends ErrorMsg:
  override def msg: String =
    val foundStr = tokenToString(found, terminalName)
    s"Syntax error. Found $foundStr, expected ${expectedStr(expected)}"

/** Empty Input: This represents an input without any token */
object EmptyInputError extends ErrorMsg:
  override def msg: String = "Input is empty!"

/** Unexpected eof */
case class InputEndedUnexpectedly(expected: Iterable[String]) extends ErrorMsg:
  override def msg: String =
    s"Input ended unexpectedly. Expected ${expectedStr(expected)}"

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
  tokenToString(token, name)

def tokenToString[Token](token: Token, name: String): String =
  val positionString = tokenPosition(token).map(pos => s" at: $pos")
  name + positionString.getOrElse("")
