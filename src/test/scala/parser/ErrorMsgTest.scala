import pudu.parser._
import pudu.parser.generator.SLRParserGenerator

class ErrorMsgTest extends munit.FunSuite {
  case class Position(line: Int, col: Int):
    override def toString = s"($line, $col)"

  enum NoPos:
    case Case1()
    case Case2(str: String)

  enum Pos:
    case Case3(p: Position)

  test("token to string no pos") {
    assertEquals(tokenToString(NoPos.Case1()), "Case1")
  }

  test("token to string pos") {
    assertEquals(tokenToString(Pos.Case3(Position(2,3))), "Case3 at: (2, 3)")
  }

  val parser = SLRParserGenerator(SimpleArithmetic).parser

  test("syntax error 1") {
    val input = "2 + * 3"
    val lexer = ArithmeticLexer.lexer(input)
    val result = parser(lexer)

    assert(result.isLeft)
    val error: ErrorMsg = result.left.get
    val msg = error.msg.span(_ != '<')
    assertEquals(msg.head, "Syntax error. Found Times, expected any of ")
    // expected tokens are not sorted, so we compare as sets
    val expected = msg.last.drop(1).dropRight(1).split(",").toSet
    assertEquals(expected, Set("IntLit", "LPar" , "FuncId"))
  }

  test("unexpected end of input") {
    val input = "2 + "
    val lexer = ArithmeticLexer.lexer(input)
    val result = parser(lexer)

    assert(result.isLeft)
    val error: ErrorMsg = result.left.get
    val msg = "Input ended unexpectedly."
    assertEquals(error.msg.take(msg.size), msg)
  }

  test("Empty input") {
    val input = " "
    val lexer = ArithmeticLexer.lexer(input)
    val result = parser(lexer)

    assert(result.isLeft)
    val error: ErrorMsg = result.left.get
    assertEquals(error.msg, "Input is empty!")
  }

  test("lexical error") {
    val input = " 2 _ 3"
    val lexer = ArithmeticLexer.lexer(input)
    val result = parser(lexer)

    assert(result.isLeft)
    val error: ErrorMsg = result.left.get
    assertEquals(error.msg, "Lexical error.")
  }
}
