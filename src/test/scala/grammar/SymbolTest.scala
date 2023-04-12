import pudu.grammar._

class SymbolTest extends munit.FunSuite {
  enum TestEnum:
    case First(x: Int, y: String)
    case Second()
    case Third(x: List[Int])

  test("create from enum") {
    val terminal1 = Terminal[TestEnum.Second]
    val terminal2 = Terminal[TestEnum.Third]
    assertEquals(terminal1.ordinal, 1)
    assertEquals(terminal2.ordinal, 2)
  }

  test("terminal toString") {
    val terminal = Terminal[TestEnum.Second]
    assertEquals(terminal.toString, "Terminal(1)")
  }

  test("non terminal toString") {
    val nt = NonTerminal[Int] ("someTag")
    assertEquals(nt.toString, "someTag")
  }
}
