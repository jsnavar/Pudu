import pudu.lexer._

class LexerTest extends munit.FunSuite {
  enum Token:
    case LPar()
    case RPar()
    case Plus()
    case Times()
    case Minus()
    case Comma()
    case FuncId(id: String)
    case IntLit(value: Int)
    case EOF()

  object ArithmeticLexer extends Lexer[Token]:

    "\\(" { Token.LPar() }
    "\\)" { Token.RPar() }
    "\\+" { Token.Plus() }
    "\\*" { Token.Times() }
    "\\-" { Token.Minus() }
    "\\," { Token.Comma() }
    "[a-z]+[a-z0-9]*" { Token.FuncId(_) }
    "[0-9]+" { s => Token.IntLit(s.toInt) }

    "[ \n]+".ignore

    // just for testing
    var ntabs = 0
    "\t+".ignore { str => ntabs += str.size }

    override val eof = Token.EOF()

  test("single token") {
    val lexer = ArithmeticLexer.lexer("+")
    assertEquals(lexer.toSeq, Seq(Token.Plus(), Token.EOF()))
  }

  test("several matches") {
    val lexer = ArithmeticLexer.lexer("32+64")
    assertEquals(lexer.toSeq, Seq(Token.IntLit(32), Token.Plus(), Token.IntLit(64), Token.EOF()))
  }

  test("ignored characters") {
    val lexer = ArithmeticLexer.lexer(" 32   \t\n\n+          \t  64")
    assertEquals(lexer.toSeq, Seq(Token.IntLit(32), Token.Plus(), Token.IntLit(64), Token.EOF()))
  }

  test("ignore function") {
    val lexer = ArithmeticLexer.lexer(" 32   \t\n\n+          \t  64")
    assertEquals(ArithmeticLexer.ntabs, 2)
  }
}
