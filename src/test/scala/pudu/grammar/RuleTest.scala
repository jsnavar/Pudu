import pudu.grammar._

class RuleTest extends munit.FunSuite {
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
    case ERROR()

  object SimpleArithmetic extends LanguageSpec[Int, Token]:
    // Symbol objects
    val expr = NonTerminal[Int]("Expr")
    val plus = Terminal[Token.Plus]

    override val start = expr
    override val eof = Terminal[Token.EOF]
    override val error = Terminal[Token.ERROR]

    // Rules
    (expr ::= (expr, plus, expr)) {
      (exp1, _, exp2) => exp1 + exp2
    }

  test("Rule toString") {
    val rule = SimpleArithmetic.grammar.rules.head

    assertEquals(rule.toString, "Expr ::= Expr Plus Expr")
  }


}
