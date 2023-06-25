import pudu.lexer._
import pudu.grammar._
import pudu.parser._
import pudu.parser.generator._

/** Tests for the interaction of lexer and parser (something like integration testing,
 * but not really).
 * As this interface is just an Iterator[Token], there is not much to test here, so we
 * just do some sanity checks. */
class IntTest extends munit.FunSuite {
  val parser = LR1ParserGenerator(SimpleArithmetic.grammar).parser
  val strParser = ArithmeticLexer.lexer.andThen(parser)

  test("eval 1") {
    val result = strParser("2 + 3 * pow(3, 5)")

    assertEquals(result, Right(731))
  }

  test("eval 2") {
    val result = strParser("10 - 2 - 3")

    assertEquals(result, Right(5))
  }

  test("syntax error") {
    val input = "10 - * 2 - 3"
    val result = strParser("10 - * 2 - 3")

    result match
      case Left(err) => assert(err.isInstanceOf[ErrorMsg.SyntaxError[_]])
      case _ => fail(s"Input '$input' should fail (syntax error)")
  }

  test("lex error") {
    val input = "10 _ 8"
    val result = strParser(input)

    result match
      case Left(err) => assertEquals(err, ErrorMsg.LexError(Token.ERROR("_")))
      case _ => fail(s"Input '$input' should fail (lex error)")
  }

  test("ended unexpectedly error") {
    val input = "10 + ( 3"
    val result = strParser(input)

    result match
      case Left(err) => assert(err.isInstanceOf[ErrorMsg.InputEndedUnexpectedly])
      case _ => fail(s"Input '$input' should fail (input ended unexpectedly error)")
  }

  test("empty string") {
    val input = ""
    val result = strParser(input)
    /* pudu.lexer.Lexer always generates an EOF token, so the error in this case
     * is an InputEndedUnexpectedly error */
    result match
      case Left(err) => assert(err.isInstanceOf[ErrorMsg.InputEndedUnexpectedly])
      case _ => fail(s"Input '$input' should fail (input ended unexpectedly error)")
  }

}
