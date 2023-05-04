import pudu.parser._

class ErrorMsgTest extends munit.FunSuite {
  case class Position(line: Int, col: Int):
    override def toString = s"($line, $col)"

  enum NoPos:
    case Case1()
    case Case2(str: String)

  enum Pos:
    case Case3(p: Position)

  test("token to string no pos") {
    assertEquals(tokenToString(NoPos.Case1()), "Case1")
  }

  test("token to string pos") {
    assertEquals(tokenToString(Pos.Case3(Position(2,3))), "Case3 at: (2, 3)")
  }

}
