import pudu.parser.generator._


class LR0ParserGeneratorTest extends munit.FunSuite {

  test("not implemented") {
    val parserGen = LR0ParserGenerator(SimpleArithmetic.grammar.augmented)
    intercept[NotImplementedError] {
      parserGen.parser(Seq(Token.EOF()).iterator)
    }
  }

}

