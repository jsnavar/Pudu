import pudu.grammar._
import pudu.parser.generator._

import scala.util.matching._

// import org.scalatest.mockito.MockitoSugar // no Scala 3 support :(
// import org.mockito.Mockito._

class LRReportTest extends munit.FunSuite {
  val gen = SLRParserGenerator(SimpleArithmetic.grammar.augmented)
  val report = gen.report

  test("number of rules") {
    val rules = gen.augmentedGrammar.rules
    assertEquals(rules.size, 1 + report.rules.count(_ == '\n'))
  }

  test("number of states") {
    val header = "State [0-9]+".r
    val count = header.findAllMatchIn(report.states).size
    val indexedStates = gen.lra.indexedStates
    assertEquals(indexedStates.size, count)
  }

  test("first size") {
    val header = "FIRST".r
    val count = header.findAllMatchIn(report.first).size
    assertEquals(gen.lrff.first.size, count)
  }

  test("follow size") {
    val header = "FOLLOW".r
    val count = header.findAllMatchIn(report.follow).size
    assertEquals(gen.lrff.follow.size, count)
  }

  test("lr automaton size") {
    assertEquals(gen.lra.lrAutomaton.size, 1 + report.lrAutomaton.count(_ == '\n'))
  }

  test("actions table size") {
    assertEquals(gen.parserGen.actionTable.size, 1 + report.actionTable.count(_ == '\n'))
  }

  test("report all without conflicts") {
    val grammar = SimpleArithmetic.grammar.augmented
    val stubRule = grammar.rules.find(r => r.left == SimpleArithmetic.exprList && r.right.size == 1).get
    val report = LRReport(grammar.rules,
                          grammar.terminalNames,
                          Map.empty,
                          Map.empty,
                          Map(Set.empty -> 0,
                              Set(stubRule.toItem(SimpleArithmetic.eof).shift) -> 1,
                              Set.empty -> 2),
                          Map.empty,
                          Map((1,1) -> Set(SRAction.Accept),
                              (1,2) -> Set(SRAction.Shift(3)),
                              (2,1) -> Set(SRAction.Reduce(stubRule)),
                              (2,3) -> Set(SRAction.Reduce(stubRule))),
                          Map((1, SimpleArithmetic.expr) -> 2))

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
    val grammar = SimpleArithmetic.grammar.augmented
    val stubRule = grammar.rules.find(r => r.left == SimpleArithmetic.exprList && r.right.size == 1).get
    val report = LRReport(grammar.rules,
                          grammar.terminalNames,
                          Map.empty,
                          Map.empty,
                          Map(Set.empty -> 0,
                              Set(stubRule.toItem(SimpleArithmetic.eof).shift) -> 1,
                              Set.empty -> 2),
                          Map.empty,
                          Map((1,1) -> Set(SRAction.Accept),
                              (1,2) -> Set(SRAction.Shift(3), SRAction.Shift(4)),
                              (2,1) -> Set(SRAction.Reduce(stubRule)),
                              (2,3) -> Set(SRAction.Reduce(stubRule))),
                          Map((1, SimpleArithmetic.expr) -> 2))

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
}
