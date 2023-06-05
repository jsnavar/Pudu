import pudu.lexer._
import pudu.grammar._
import pudu.parser._
import pudu.parser.generator._

class IntTest extends munit.FunSuite {
  val parser = LR1ParserGenerator(SimpleArithmetic).parser

  test("eval 1") {
    val input = "2 + 3 * pow(3, 5)"
    val lexer = ArithmeticLexer.lexer(input)
    val result = parser(lexer)

    assert(result.isRight)
    assertEquals(result.getOrElse(0), 731)
  }

  test("eval 2") {
    val input = "10 - 2 - 3"
    val lexer = ArithmeticLexer.lexer(input)
    val result = parser(lexer)

    assert(result.isRight)
    assertEquals(result.getOrElse(0), 5)
  }

  test("error 1") {
    val input = "10 - * 2 - 3"
    val lexer = ArithmeticLexer.lexer(input)
    val result = parser(lexer)

    assert(result.isLeft)
  }

  test("error 2") {
    val input = "10 _ 8"
    val lexer = ArithmeticLexer.lexer(input)
    val result = parser(lexer)

    assert(result.isLeft)
    assertEquals(result.swap.toOption.get, ErrorMsg.LexError(Token.ERROR("_")))
  }
}
