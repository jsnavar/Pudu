import pudu.grammar._
import pudu.parser._
import pudu.parser.generator._


class SLRParserGeneratorTest extends munit.FunSuite {
  val parser = SLRParserGenerator(SimpleArithmetic).parser

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
}
