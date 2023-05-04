package pudu.parser

abstract class ErrorMsg:
  def msg: String

class SyntaxError[Token](last: Token) extends ErrorMsg:
  override def msg: String = ???

object EmptyInputError extends SyntaxError[Unit](()):
  override def msg: String = ???

class InputEndedUnexpectedly[Token](last: Token) extends SyntaxError(last):
  override def msg: String = ???

class LexError[Token](seen: Token) extends ErrorMsg:
  override def msg: String = s"Lexical error"

def tokenToString[Token](token: Token): String =
  val name = token.getClass().getName().split('$').last
  var posString = ""
  try {
    posString = " at: " + token.getClass().getMethod("p").invoke(token).toString
  } catch { _ => () }
  name + posString
