import pudu.grammar._
import pudu.parser.generator._

import scala.util.matching._

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

  test("lr0 automaton size") {
    assertEquals(gen.lr0Automaton.size, 1 + report.lr0Automaton.count(_ == '\n'))
  }

  test("actions table size") {
    assertEquals(gen.actionTable.size, 1 + report.actionTable.count(_ == '\n'))
  }
}
