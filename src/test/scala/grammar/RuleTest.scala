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

  object SimpleArithmetic extends LanguageSpec[Int, Token]:
    // Symbol objects
    val expr = NonTerminal[Int]("Expr")
    val plus = Terminal[Token.Plus]

    override val start = expr
    override val eof = Terminal[Token.EOF]

    // Rules
    (expr ::= (expr, plus, expr)) {
      (exp1, _, exp2) => exp1 + exp2
    }

  test("Rule toString") {
    val rule = SimpleArithmetic.rules.head

    assertEquals(rule.toString, "Expr ::= Expr Terminal(2) Expr")
  }


}
