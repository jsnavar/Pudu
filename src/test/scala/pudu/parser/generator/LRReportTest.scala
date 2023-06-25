import pudu.grammar._
import pudu.parser.generator._

import scala.util.matching._

// import org.scalatest.mockito.MockitoSugar // no Scala 3 support :(
// import org.mockito.Mockito._

class LRReportTest extends munit.FunSuite {/*
  val gen = SLRParserGenerator(SimpleArithmetic.grammar.augmented)
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

  test("report all without conflicts") {
    val stub = new LRParserGenerator(SimpleArithmetic.grammar.augmented) {
      override def closure(state: StateT): StateT = Set.empty
      override val startState = Set.empty
      override def parser = ???
      val eof = SimpleArithmetic.grammar.eof
      lazy val reduceActions: Map[(StateT, Terminal[Token]), Set[RuleT]] = Map.empty
      override lazy val lrAutomaton: Map[(StateT, Symbol), StateT] = Map.empty

      val stubRule = rules.find(r => r.left == SimpleArithmetic.exprList && r.right.size == 1).get
      override lazy val indexedStates = Map(Set.empty -> 0,
                                            Set(stubRule.toItem(eof).shift) -> 1,
                                            Set.empty -> 2)
      override lazy val actionTable = Map((1,1) -> Set(SRAction.Accept),
                                          (1,2) -> Set(SRAction.Shift(3)),
                                          (2,1) -> Set(SRAction.Reduce(stubRule)),
                                          (2,3) -> Set(SRAction.Reduce(stubRule)))
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

  test("report all with conflicts") {
    val stub = new LRParserGenerator(SimpleArithmetic.grammar.augmented) {
      override def closure(state: StateT): StateT = Set.empty
      override val startState = Set.empty
      override def parser = ???
      val eof = SimpleArithmetic.grammar.eof
      lazy val reduceActions: Map[(StateT, Terminal[Token]), Set[RuleT]] = Map.empty
      override lazy val lrAutomaton: Map[(StateT, Symbol), StateT] = Map.empty

      val stubRule = rules.find(r => r.left == SimpleArithmetic.exprList && r.right.size == 1).get
      override lazy val indexedStates = Map(Set.empty -> 0,
                                            Set(stubRule.toItem(eof).shift) -> 1,
                                            Set.empty -> 2)
      override lazy val actionTable = Map((1,1) -> Set(SRAction.Accept),
                                          (1,2) -> Set(SRAction.Shift(3), SRAction.Shift(4)),
                                          (2,1) -> Set(SRAction.Reduce(stubRule)),
                                          (2,3) -> Set(SRAction.Reduce(stubRule)))
      override lazy val gotoTable = Map((1, SimpleArithmetic.expr) -> 2)
    }

    val report = LRReport(stub)
    val template = """	State 1:
		ExprList ::= Expr ·  | EOF
		---
		on RPar --> Accept
		on Plus --> [Shift(<v1>), Shift(<v2>)]
		---
		if Expr goto 2

	State 2:
		on RPar --> Reduce(ExprList ::= Expr)
		on Times --> Reduce(ExprList ::= Expr)"""
    val alternatives = Set(template.replace("<v1>", "3").replace("<v2>", "4"),
                           template.replace("<v1>", "4").replace("<v2>", "3"))
    assert(alternatives.contains(report.all))
  }
*/

}
