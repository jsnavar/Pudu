import pudu.parser.generator._

class LRAutomatonTest extends munit.FunSuite {
  import SimpleArithmetic._
  import ArithmeticTest._

  val closure = LR0Closure(rules).closure
  val startState = closure(ag.startRules.map(_.toItem))
  val lra = LRAutomaton(startState, closure)

  test("GOTO emtpy") {
    // expr ::= funcId lpar . exprList rpar
    val state = select(_.right.head == funcId).map(_.shift.shift)
    assertEquals(lra.goto(state, comma), Set())
  }
  test("GOTO same") {
    // expr ::= funcId lpar . exprList rpar
    val state = select(_.right.head == funcId).map(_.shift.shift)
    assertEquals(lra.goto(state, exprList), state.map(_.shift))
  }

  test("GOTO 3") {
    // expr ::= expr . times expr
    val state = select(_.right.contains(times)).map(_.shift)

    assertEquals(lra.goto(state, times), state.map(_.shift) ++ rules.filter(_.left == expr).map(_.toItem))
  }
  test("GOTO all") {
    val state = select(r => r.left == expr && r.right.head == expr)
      .map(_.shift)
    assertEquals(state.size, 3)

    val gotoAll = lra.goto(state)
    assertEquals(gotoAll.size, 3)
    assertEquals(gotoAll.keys.toSet, Set((plus), (minus), (times)))

    assertEquals(gotoAll(plus).toSet, lra.goto(state, plus))
    assertEquals(gotoAll(minus).toSet, lra.goto(state, minus))
    assertEquals(gotoAll(times).toSet, lra.goto(state, times))
  }
  test("lrAutomaton") {
    val automaton = lra.lrAutomaton

    val toExpr = lra.goto(startState, expr)
    assertEquals(automaton(startState, expr), toExpr)

    val toFuncId = lra.goto(startState, funcId)
    assertEquals(automaton(startState, funcId), toFuncId)
    val toLPar = lra.goto(toFuncId, lpar)
    assertEquals(automaton(toFuncId, lpar), toLPar)
  }
}
