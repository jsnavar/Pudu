import pudu.grammar._
import pudu.parser._
import pudu.parser.generator._

class SLRParserGeneratorTest extends munit.FunSuite {
  val grammar = SimpleArithmetic.grammar.augmented

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

  test("Empty precedence shifts") {
    object Arithmetic extends LanguageSpec[Int, Token]:
      val expr = NonTerminal[Int]("expr")
      val times = Terminal[Token.Times]
      val plus = Terminal[Token.Plus]
      val intLit = Terminal[Token.IntLit]

      override val eof = Terminal[Token.EOF]
      override val error = Terminal[Token.ERROR]
      override val start = expr
      override val precedence = Precedence.empty

      (expr ::= intLit) { _.value }
      (expr ::= (expr, plus, expr)) { (l, _, r) => l + r }
      (expr ::= (expr, times, expr)) { (l, _, r) => l * r }

    val parser = SLRParserGenerator(grammar).parser
    val input = Seq(Token.IntLit(2), Token.Plus(), Token.IntLit(3), Token.Times(), Token.IntLit(4), Token.EOF())

    val result = parser(input.iterator)
    assertEquals(result.getOrElse(0), 14)
  }

  test("Precedence equal level left") {
    object Arithmetic extends LanguageSpec[Int, Token]:
      val expr = NonTerminal[Int]("expr")
      val times = Terminal[Token.Times]
      val plus = Terminal[Token.Plus]
      val intLit = Terminal[Token.IntLit]

      override val eof = Terminal[Token.EOF]
      override val error = Terminal[Token.ERROR]
      override val start = expr
      override val precedence = Precedence().left(times, plus)

      (expr ::= intLit) { _.value }
      (expr ::= (expr, plus, expr)) { (l, _, r) => l + r }
      (expr ::= (expr, times, expr)) { (l, _, r) => l * r }

    val parser = SLRParserGenerator(Arithmetic.grammar.augmented).parser
    val input = Seq(Token.IntLit(2), Token.Plus(), Token.IntLit(3), Token.Times(), Token.IntLit(4), Token.EOF())

    val result = parser(input.iterator)
    assertEquals(result.getOrElse(0), 20)
  }

  test("Precedence equal level right") {
    object Arithmetic extends LanguageSpec[Int, Token]:
      val expr = NonTerminal[Int]("expr")
      val times = Terminal[Token.Times]
      val plus = Terminal[Token.Plus]
      val intLit = Terminal[Token.IntLit]

      override val eof = Terminal[Token.EOF]
      override val error = Terminal[Token.ERROR]
      override val start = expr
      override val precedence = Precedence().right(times, plus)

      (expr ::= intLit) { _.value }
      (expr ::= (expr, plus, expr)) { (l, _, r) => l + r }
      (expr ::= (expr, times, expr)) { (l, _, r) => l * r }

    val parser = SLRParserGenerator(Arithmetic.grammar.augmented).parser
    val input = Seq(Token.IntLit(2), Token.Plus(), Token.IntLit(3), Token.Times(), Token.IntLit(4), Token.EOF())

    val result = parser(input.iterator)
    assertEquals(result.getOrElse(0), 14)
  }
  test("Precedence equal level nonassoc") {
    object Arithmetic extends LanguageSpec[Int, Token]:
      val expr = NonTerminal[Int]("expr")
      val times = Terminal[Token.Times]
      val plus = Terminal[Token.Plus]
      val intLit = Terminal[Token.IntLit]

      override val eof = Terminal[Token.EOF]
      override val error = Terminal[Token.ERROR]
      override val start = expr
      override val precedence = Precedence().nonassoc(times, plus)

      (expr ::= intLit) { _.value }
      (expr ::= (expr, plus, expr)) { (l, _, r) => l + r }
      (expr ::= (expr, times, expr)) { (l, _, r) => l * r }

    val parserGen = SLRParserGenerator(Arithmetic.grammar.augmented)
    val actions = parserGen.actionTable

    assert(actions.find(entry => entry._2.size == 2).isDefined)
  }

  test("Precedence rule without terminal") {
    object Arithmetic extends LanguageSpec[Int, Token]:
      val expr = NonTerminal[Int]("expr")
      val binop = NonTerminal[Int]("op")

      val times = Terminal[Token.Times]
      val plus = Terminal[Token.Plus]
      val intLit = Terminal[Token.IntLit]

      override val eof = Terminal[Token.EOF]
      override val error = Terminal[Token.ERROR]
      override val start = expr
      override val precedence = Precedence()

      (binop ::= plus) { _ => 0 }
      (binop ::= times) { _ => 1 }

      (expr ::= intLit) { _.value }
      (expr ::= (expr, binop, expr)) { (l, op, r) => if op == 0 then l + r else l * r }

    val parser = SLRParserGenerator(Arithmetic.grammar.augmented).parser
    val input = Seq(Token.IntLit(2), Token.Plus(), Token.IntLit(3), Token.Times(), Token.IntLit(4), Token.EOF())

    val result = parser(input.iterator)
    assertEquals(result.getOrElse(0), 14)
  }

  test("Manual precedence 1") {
    object Arithmetic extends LanguageSpec[Int, Token]:
      val exprAdd = NonTerminal[Int]("expr+")
      val exprMul = NonTerminal[Int]("exprx")

      val times = Terminal[Token.Times]
      val plus = Terminal[Token.Plus]
      val intLit = Terminal[Token.IntLit]

      override val eof = Terminal[Token.EOF]
      override val error = Terminal[Token.ERROR]
      override val start = exprAdd

      (exprAdd ::= exprMul) { identity }
      (exprAdd ::= (exprAdd, plus, exprMul)) { (l,_,r) => l + r }
      (exprMul ::= intLit) { _.value }
      (exprMul ::= (exprMul, times, intLit)) { (l,_,r) => l * r.value }

    val parser = SLRParserGenerator(Arithmetic.grammar.augmented).parser
    val input = Seq(Token.IntLit(2), Token.Plus(), Token.IntLit(3), Token.Times(), Token.IntLit(4), Token.EOF())

    val result = parser(input.iterator)
    assertEquals(result.getOrElse(0), 14)
  }
  test("Manual precedence 2 (right)") {
    object Arithmetic extends LanguageSpec[Int, Token]:
      val expr = NonTerminal[Int]("expr")

      val times = Terminal[Token.Times]
      val plus = Terminal[Token.Plus]
      val intLit = Terminal[Token.IntLit]

      override val eof = Terminal[Token.EOF]
      override val error = Terminal[Token.ERROR]
      override val start = expr

      (expr ::= intLit) { _.value }
      (expr ::= (intLit, plus, expr)) { (l,_,r) => l.value + r }
      (expr ::= (intLit, times, expr)) { (l,_,r) => l.value * r }

    val parser = SLRParserGenerator(Arithmetic.grammar.augmented).parser
    val input = Seq(Token.IntLit(2), Token.Plus(), Token.IntLit(3), Token.Times(), Token.IntLit(4), Token.EOF())

    val result = parser(input.iterator)
    assertEquals(result.getOrElse(0), 14)
  }
  test("Manual precedence 3 (left)") {
    object Arithmetic extends LanguageSpec[Int, Token]:
      val expr = NonTerminal[Int]("expr")

      val times = Terminal[Token.Times]
      val plus = Terminal[Token.Plus]
      val intLit = Terminal[Token.IntLit]

      override val eof = Terminal[Token.EOF]
      override val error = Terminal[Token.ERROR]
      override val start = expr

      (expr ::= intLit) { _.value }
      (expr ::= (expr, plus, intLit)) { (l,_,r) => l + r.value }
      (expr ::= (expr, times, intLit)) { (l,_,r) => l * r.value }

    val parser = SLRParserGenerator(Arithmetic.grammar.augmented).parser
    val input = Seq(Token.IntLit(2), Token.Plus(), Token.IntLit(3), Token.Times(), Token.IntLit(4), Token.EOF())

    val result = parser(input.iterator)
    assertEquals(result.getOrElse(0), 20)
  }

  test("RR conflict") {
    object Arithmetic extends LanguageSpec[Int, Token]:
      val expr = NonTerminal[Int]("expr")

      val times = Terminal[Token.Times]
      val plus = Terminal[Token.Plus]
      val intLit = Terminal[Token.IntLit]

      override val eof = Terminal[Token.EOF]
      override val error = Terminal[Token.ERROR]
      override val start = expr

      (expr ::= intLit) { _.value }
      (expr ::= (expr, plus, expr)) { (l,_,r) => l + r }
      (expr ::= (expr, plus, expr)) { (l,_,r) => l * r }

    val parserGen = SLRParserGenerator(Arithmetic.grammar.augmented)
    val actions = parserGen.actionTable

    assert(actions.find(entry => entry._2.size == 2).isDefined)
    assert(actions.find(entry => entry._2.size == 3).isDefined)
  }

  test("RR conflict report") {
    object Arithmetic extends LanguageSpec[Int, Token]:
      val expr = NonTerminal[Int]("expr")

      val times = Terminal[Token.Times]
      val plus = Terminal[Token.Plus]
      val intLit = Terminal[Token.IntLit]

      override val eof = Terminal[Token.EOF]
      override val error = Terminal[Token.ERROR]
      override val start = expr

      (expr ::= intLit) { _.value }
      (expr ::= (expr, plus, expr)) { (l,_,r) => l + r }
      (expr ::= (expr, plus, expr)) { (l,_,r) => l * r }

    val ex = intercept[UnresolvedConflictException] {
      val parser = SLRParserGenerator(Arithmetic.grammar.augmented).parser
    }
    assert(ex.getMessage().startsWith("Unresolved conflicts. Report written to"))
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
