import pudu.parser._
import pudu.parser.generator.SLRParserGenerator

class ErrorMsgTest extends munit.FunSuite {
  import ErrorMsg._

  case class Position(line: Int, col: Int):
    override def toString = s"($line, $col)"

  enum NoPos:
    case Case1()
    case Case2(str: String)

  enum Pos:
    case Case3(p: Position)
    case Case4(str: String, p: Position)

  test("syntax errors without positions") {
    val err0 = ErrorMsg.SyntaxError(NoPos.Case1(), "Name", Seq.empty)
    assertEquals(err0.msg, "Syntax error. Found Name, expected nothing (??)")

    val err1 = ErrorMsg.SyntaxError(NoPos.Case1(), "Name", Seq("xyz"))
    assertEquals(err1.msg, "Syntax error. Found Name, expected xyz")

    val err3 = ErrorMsg.SyntaxError(NoPos.Case1(), "Name", Seq("a", "b", "c"))
    assertEquals(err3.msg, "Syntax error. Found Name, expected any of <a,b,c>")
  }

  test("syntax errors with positions") {
    val tok = Pos.Case3(Position(2,3))
    val err0 = ErrorMsg.SyntaxError(tok, "Token", Seq.empty)
    assertEquals(err0.msg, "Syntax error. Found Token (position: (2, 3)), expected nothing (??)")

    val err1 = ErrorMsg.SyntaxError(tok, "Name", Seq("xyz"))
    assertEquals(err1.msg, "Syntax error. Found Name (position: (2, 3)), expected xyz")

    val err3 = ErrorMsg.SyntaxError(tok, "Name", Seq("a", "b", "c"))
    assertEquals(err3.msg, "Syntax error. Found Name (position: (2, 3)), expected any of <a,b,c>")
  }

  test("empty input") {
    assertEquals(ErrorMsg.EmptyInputError.msg, "Input is empty!")
  }

  test("input ended unexpectedly") {
    assertEquals(ErrorMsg.InputEndedUnexpectedly(Seq.empty).msg, "Input ended unexpectedly. Expected nothing (??)")
    assertEquals(ErrorMsg.InputEndedUnexpectedly(Seq("xyz")).msg, "Input ended unexpectedly. Expected xyz")
    assertEquals(ErrorMsg.InputEndedUnexpectedly(Seq("a","b","c","d")).msg, "Input ended unexpectedly. Expected any of <a,b,c,d>")
  }

  test("lex error") {
    assertEquals(ErrorMsg.LexError(NoPos.Case1()).msg, "Unrecognized input")
    assertEquals(ErrorMsg.LexError(NoPos.Case2("x")).msg, "Unrecognized input 'x'")
    assertEquals(ErrorMsg.LexError(Pos.Case3(Position(1,2))).msg, "Unrecognized input (position: (1, 2))")
    assertEquals(ErrorMsg.LexError(Pos.Case4("x", Position(1,2))).msg, "Unrecognized input 'x' (position: (1, 2))")
  }

}
