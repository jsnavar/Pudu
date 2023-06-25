import pudu.parser.generator._

class LR0ClosureTest extends munit.FunSuite {
  import ArithmeticTest._
  import SimpleArithmetic._

  val lrc = LR0Closure(rules)

  test("Closure 1") {
    val state = select(_.right == Seq(expr))
    val candidates = rules.map(_.toItem)

    val res = lrc.closure(state)

    val diff = candidates -- res

    val expDiff = select(_.right.contains(comma))
    assertEquals(diff, expDiff)
    assertEquals(diff.size, 1)
    assertEquals(diff.head.after, Seq(expr, comma, exprList))
  }

  test("Closure 2") {
    // expr ::= funcId lpar . exprList rpar
    val startItem = select(_.right.head == funcId).map(_.shift.shift).head

    val cls = lrc.closure(Set(startItem))

    assertEquals(cls.map(_.toString).mkString("\n"), (rules.map(_.toItem).filter(_.left != ag.startSymbol) + startItem).map(_.toString).mkString("\n"))
  }
}
