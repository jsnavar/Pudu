package pudu.parser

import scala.util.{Try, Success, Failure}
import pudu.lexer.Position

abstract class ErrorMsg:
  def msg: String

class SyntaxError[Token](val found: Token, val expected: Iterable[String]) extends ErrorMsg:
  override def msg: String = 
    val foundStr = tokenToString(found)
    val expectedStr = if expected.isEmpty then "nothing (??)"
      else if expected.size == 1 then expected.head.toString
      else s"any of <${expected.mkString(','.toString)}>"
    s"Syntax error. Found $foundStr, expected $expectedStr"

object EmptyInputError extends SyntaxError[Unit]((), Set.empty):
  override def msg: String = "Input is empty!"

class InputEndedUnexpectedly[Token](last: Token) extends SyntaxError(last, Set.empty):
  override def msg: String =
    val lastStr = tokenToString(last)
    s"Input ended unexpectedly. Last token was $lastStr"

class LexError[Token](found: Token) extends ErrorMsg:
  override def msg: String =
    val positionString = tokenPosition(found).map(pos => s" at: $pos")
    "Lexical error" + positionString.getOrElse(".")

def tokenPosition[Token](token: Token): Option[Any] =
  Try(token.getClass().getMethod("p").invoke(token)).toOption

def tokenToString[Token](token: Token): String =
  val name = token.getClass().getName().split('$').last
  val positionString = tokenPosition(token).map(pos => s" at: $pos")
  name + positionString.getOrElse("")
