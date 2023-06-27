import pudu.parser.generator._
import pudu.grammar.Symbol

class LRItemTest extends munit.FunSuite {
  import SimpleArithmetic._
  import ArithmeticTest._

  test("Item equality") {
    val rule = rules.filter(_.right == Seq(expr)).head

    assertEquals(rule.toItem, rule.toItem)
  }

  test("Item shift once") {
    val filtered = rules.filter(_.left == exprList)
    assertEquals(filtered.size, 2)

    val shifted = filtered.map(_.toItem).map(_.shift)

    val before = shifted.map(_.before)
    assertEquals(before, Set(Seq(expr)))

    val after = shifted.map(_.after).toSet
    assertEquals(after, Set(Seq.empty[Symbol], Seq(comma, exprList)))
  }

  test("Item shift twice") {
    val filtered = select(_.left == exprList)
      .map(_.shift)
      .filterNot(_.after.isEmpty).map(_.shift)

    assertEquals(filtered.size, 1)
    val item = filtered.head
    assertEquals(item.before, Seq(expr, comma))
    assertEquals(item.after, Seq(exprList))
  }

  test("State equality") {
    val state1 = select(_.left == exprList)
      .map(_.shift)
    val state2 = select(_.left == exprList)
      .map(_.shift)

    assertEquals(state1, state2)
  }

}
