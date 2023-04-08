import pudu.grammar._
import pudu.parser.generator._

class LrCommonTest extends munit.FunSuite {
  import SimpleArithmetic._
  object TestLR extends LRParserGenerator(SimpleArithmetic):
      def apply: Iterator[Token] => Int = ???

  def select(f: TestLR.RuleT => Boolean): TestLR.State =
    rules.filter(f).map(_.toItem)

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

  test("Closure 1") {
    val state = select(_.right == Seq(expr))
    val candidates = rules.map(_.toItem)

    val res = TestLR.stateClosure(state, candidates)

    val diff = candidates -- res

    val expDiff = select(_.right.contains(comma))
    assertEquals(diff, expDiff)
    assertEquals(diff.size, 1)
    assertEquals(diff.head.after, Seq(expr, comma, exprList))
  }

  test("Closure 2") {
    // expr ::= funcId lpar . exprList rpar
    val startItem = select(_.right.head == funcId).map(_.shift.shift).head

    val cls = TestLR.closure(Set(startItem))

    assertEquals(cls, rules.map(_.toItem) + startItem)
  }

  test("GOTO emtpy") {
    // expr ::= funcId lpar . exprList rpar
    val state = select(_.right.head == funcId).map(_.shift.shift)
    assertEquals(TestLR.goto(state, comma), Set())
  }
  test("GOTO same") {
    // expr ::= funcId lpar . exprList rpar
    val state = select(_.right.head == funcId).map(_.shift.shift)
    assertEquals(TestLR.goto(state, exprList), state.map(_.shift))
  }

  test("GOTO 3") {
    // expr ::= expr . times expr
    val state = select(_.right.contains(times)).map(_.shift)

    assertEquals(TestLR.goto(state, times), state.map(_.shift) ++ rules.filter(_.left == expr).map(_.toItem))
  }
  test("GOTO all") {
    val state = select(r => r.left == expr && r.right.head == expr)
      .map(_.shift)
    assertEquals(state.size, 4)

    val gotoAll = TestLR.goto(state)
    assertEquals(gotoAll.size, 4)
    assertEquals(gotoAll.keys.toSet, Set(plus, minus, times, eofTerminal))

    assertEquals(gotoAll(plus).toSet, TestLR.goto(state, plus))
    assertEquals(gotoAll(minus).toSet, TestLR.goto(state, minus))
    assertEquals(gotoAll(times).toSet, TestLR.goto(state, times))
  }
  test("first") {
    val first = TestLR.first
    assertEquals(first(expr), Set(intLit, funcId, lpar))
    assertEquals(first(exprList), Set(intLit, funcId, lpar))
    assertEquals(first(plus), Set(plus))
  }

  test("follow") {
    val follow = TestLR.follow
    assertEquals(follow(expr), Set(rpar, plus, times, minus, comma, eof))
    assertEquals(Set(rpar), follow(exprList))
  }
}
