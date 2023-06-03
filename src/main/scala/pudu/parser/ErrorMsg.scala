package pudu.parser

import scala.util.{Try, Success, Failure}

trait ErrorMsg:
  def msg: String

object ErrorMsg:
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
    val foundStr: String = Try(found.getClass().getMethod("str").invoke(found)) match
      case Success(s) => s" '$s'"
      case Failure(_) => ""
    override def msg: String =
      s"Unrecognized input${foundStr}${positionString(found)}"

  private def expectedStr(expected: Iterable[String]) =
    if expected.isEmpty then "nothing (??)"
    else if expected.size == 1 then expected.head.toString
    else
      val alternatives = expected.mkString(",")
      s"any of <$alternatives>"

  private def positionString[Token](token: Token): String =
    tokenPosition(token) match
      case Some(pos) => s" (position: $pos)"
      case None => ""

  /** Gets the position of the token. The position is obtained by invoking
   *  a parameterless method named 'p' of the token, if present. */
  private def tokenPosition[Token](token: Token): Option[Any] =
    Try(token.getClass().getMethod("p").invoke(token)).toOption

  /** String representation of a token, with position if defined */
  private def tokenToString[Token](token: Token, name: String): String =
    name + positionString(token)
