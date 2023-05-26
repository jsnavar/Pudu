import pudu.grammar._

// Tests for rule definitions

class PrecedenceTest extends munit.FunSuite {
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

  val plus   = Terminal[Token.Plus]
  val times  = Terminal[Token.Times]
  val minus  = Terminal[Token.Minus]
  val comma  = Terminal[Token.Comma]

  test("undefined") {
    val p = Precedence().left(plus, minus)
    val empty = Precedence.empty

    assertEquals(empty.max(plus, times), Side.Neither)

    assertEquals(p.max(times, plus), Side.Neither)
    assertEquals(p.max(plus, times), Side.Neither)
    assertEquals(p.max(times, times), Side.Neither)
  }
  test("undefined left") {
    val p = Precedence().left(plus)
    assertEquals(p.max(plus, times), Side.Neither)
  }
  test("undefined right") {
    val p = Precedence().left(plus)
    assertEquals(p.max(times, plus), Side.Neither)
  }
  test("same level") {
    val left = Precedence().right(times).left(plus, minus)
    val right = Precedence().right(plus, minus)
    val nonassoc = Precedence().nonassoc(plus, minus)

    assertEquals(left.max(plus, minus), Side.Left)
    assertEquals(right.max(plus, minus), Side.Right)
    assertEquals(nonassoc.max(plus, minus), Side.Error)
  }
  test("different level") {
    val p = Precedence().nonassoc(plus, minus).nonassoc(comma).right(times)

    assertEquals(p.max(plus, times), Side.Right)
    assertEquals(p.max(times, plus), Side.Left)
  }
  test("multiple declarations") {
    intercept[java.lang.IllegalArgumentException] {
      val p = Precedence().left(plus).right(plus)
    }
  }
}
