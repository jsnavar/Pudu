import pudu.grammar._
import pudu.parser.generator._

import scala.util.matching._

// import org.scalatest.mockito.MockitoSugar // no Scala 3 support :(
// import org.mockito.Mockito._

class LRReportTest extends munit.FunSuite {
  val gen = SLRParserGenerator(SimpleArithmetic)
  val report = LRReport(gen)

  test("number of rules") {
    assertEquals(gen.rules.size, 1 + report.rules.count(_ == '\n'))
  }

  test("number of states") {
    val header = "State [0-9]+".r
    val count = header.findAllMatchIn(report.states).size
    assertEquals(gen.indexedStates.size, count)
  }

  test("first size") {
    val header = "FIRST".r
    val count = header.findAllMatchIn(report.first).size
    assertEquals(gen.first.size, count)
  }

  test("follow size") {
    val header = "FOLLOW".r
    val count = header.findAllMatchIn(report.follow).size
    assertEquals(gen.follow.size, count)
  }

  test("lr automaton size") {
    assertEquals(gen.lrAutomaton.size, 1 + report.lrAutomaton.count(_ == '\n'))
  }

  test("actions table size") {
    assertEquals(gen.actionTable.size, 1 + report.actionTable.count(_ == '\n'))
  }

  test("report all") {
    val stub = new LRParserGenerator(SimpleArithmetic) {
      override def closure(state: State): State = Set.empty
      override val startState = Set.empty
      override def parser = ???
      lazy val reduceActions: Map[(State, Terminal[Token]), RuleT] = Map.empty
      override lazy val lrAutomaton: Map[(State, Symbol), State] = Map.empty

      val stubRule = rules.find(r => r.left == SimpleArithmetic.exprList && r.right.size == 1).get
      override lazy val indexedStates = Map(Set.empty -> 0,
                                            Set(stubRule.toItem(eof).shift) -> 1,
                                            Set.empty -> 2)
      override lazy val actionTable = Map((1,1) -> SRAction.Accept,
                                          (1,2) -> SRAction.Shift(3),
                                          (2,1) -> SRAction.Reduce(stubRule),
                                          (2,3) -> SRAction.Reduce(stubRule))
      override lazy val gotoTable = Map((1, SimpleArithmetic.expr) -> 2)
    }

    val report = LRReport(stub)
    val expected = """	State 1:
		ExprList ::= Expr ·  | EOF
		---
		on RPar --> Accept
		on Plus --> Shift(3)
		---
		if Expr goto 2

	State 2:
		on RPar --> Reduce(ExprList ::= Expr)
		on Times --> Reduce(ExprList ::= Expr)"""
    assertEquals(report.all, expected)
  }

}
