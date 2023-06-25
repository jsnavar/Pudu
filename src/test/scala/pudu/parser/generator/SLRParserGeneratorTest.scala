import pudu.grammar._
import pudu.parser._
import pudu.parser.generator._

class SLRParserGeneratorTest extends munit.FunSuite {
  val grammar = SimpleArithmetic.grammar

  val parser = SLRParserGenerator(grammar).parser

  test("Parse 1") {
    val input = Seq(Token.IntLit(4), Token.Minus(), Token.IntLit(5), Token.EOF())

    val result = parser(input.iterator)
    assert(result.isRight)
    assertEquals(result.getOrElse(0), -1)
  }

  test("Parse 2") {
    // 10 * pow((26-20)*2,5-3)
    val input = Seq(Token.IntLit(10), Token.Times(),
                    Token.FuncId("pow"), Token.LPar(), Token.LPar(), Token.IntLit(26), Token.Minus(), Token.IntLit(20), Token.RPar(), Token.Times(), Token.IntLit(2),
                    Token.Comma(), Token.IntLit(5), Token.Minus(), Token.IntLit(3), Token.RPar(), Token.EOF())
    val result = parser(input.iterator)
    assert(result.isRight)
    assertEquals(result.getOrElse(0), 1440)
  }

  test("Precedence") {
    val input = Seq(Token.IntLit(2), Token.Plus(), Token.IntLit(3), Token.Times(), Token.IntLit(4), Token.EOF())
    val result = parser(input.iterator)
    assert(result.isRight)
    assertEquals(result.getOrElse(0), 14)
  }

  test("syntax error") {
    val input = "2 + * 3"
    val lexer = ArithmeticLexer.lexer(input)
    val result = parser(lexer)

    assert(result.isLeft)
    val error: ErrorMsg = result.swap.toOption.get
    assert(error.isInstanceOf[ErrorMsg.SyntaxError[_]])
    val syntaxError = error.asInstanceOf[ErrorMsg.SyntaxError[Token]]
    assertEquals(syntaxError, ErrorMsg.SyntaxError(Token.Times(), "Times", Set("IntLit", "LPar" , "FuncId")))
  }

  test("unexpected end of input") {
    val input = "2 + "
    val lexer = ArithmeticLexer.lexer(input)
    val result = parser(lexer)

    assert(result.isLeft)
    val error: ErrorMsg = result.swap.toOption.get
    assert(error.isInstanceOf[ErrorMsg.InputEndedUnexpectedly])
  }

  test("Empty input") {
    val result = parser(Iterator.empty[Token])

    assert(result.isLeft)
    val error: ErrorMsg = result.swap.toOption.get
    assertEquals(error, ErrorMsg.EmptyInputError)
  }

  test("lexical error") {
    val input = " 2 _ 3"
    val lexer = ArithmeticLexer.lexer(input)
    val result = parser(lexer)

    assert(result.isLeft)
    val error: ErrorMsg = result.swap.toOption.get
    assertEquals(error, ErrorMsg.LexError(Token.ERROR("_")))
  }
}
