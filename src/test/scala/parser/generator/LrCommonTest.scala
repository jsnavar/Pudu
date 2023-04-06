import pudu.grammar._
import pudu.parser.generator._

class LrCommonTest extends munit.FunSuite {
  object TestLR extends LRParserGenerator[Int|List[Int], Token]

  test("Item equality") {
    import SimpleArithmetic._

    val rule = rules.filter(_.right == Seq(expr)).head

    assertEquals(rule.toItem, rule.toItem)
  }

  test("Item shift once") {
    import SimpleArithmetic._

    val filtered = rules.filter(_.left == exprList)
    assertEquals(filtered.size, 2)

    val shifted = filtered.map(_.toItem).map(_.shift)

    val before = shifted.map(_.before)
    assertEquals(before, Set(Seq(expr)))

    val after = shifted.map(_.after).toSet
    assertEquals(after, Set(Seq.empty[Symbol], Seq(comma, exprList)))
  }

  test("Item shift twice") {
    import SimpleArithmetic._

    val filtered = rules.filter(_.left == exprList)
                          .map(_.toItem).map(_.shift)
                          .filterNot(_.after.isEmpty).map(_.shift)

    assertEquals(filtered.size, 1)
    val item = filtered.head
    assertEquals(item.before, Seq(expr, comma))
    assertEquals(item.after, Seq(exprList))
  }

  test("Closure 1") {
    import SimpleArithmetic._

    val state = Set(rules.filter(_.right == Seq(expr)).head.toItem)
    val candidates = rules.map(_.toItem)

    val res = TestLR.stateClosure(state, candidates)

    val diff = candidates -- res

    val expDiff = rules.filter(_.right.contains(comma)).map(_.toItem)
    assertEquals(diff, expDiff)
    assertEquals(diff.size, 1)
    assertEquals(diff.head.after, Seq(expr, comma, exprList))
  }

  test("Closure 2") {
    import SimpleArithmetic._
    // expr ::= funcId lpar . exprList rpar
    val startItem = rules.filter(_.right.head == funcId).map(_.toItem.shift.shift).head

    val cls = TestLR.closure(rules)(Set(startItem))

    assertEquals(cls, rules.map(_.toItem) + startItem)
  }
}
