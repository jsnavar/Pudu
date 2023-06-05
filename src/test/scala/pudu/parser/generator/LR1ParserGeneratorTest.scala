import pudu.grammar._
import pudu.parser._
import pudu.parser.generator._

class LR1ParserGeneratorTest extends munit.FunSuite {

  /* example from dragon book */
  enum TwoToken:
    case C()
    case D()
    case EOF()
    case ERROR()

  object Example extends LanguageSpec[String, TwoToken]:
    val c = Terminal[TwoToken.C]("c")
    val d = Terminal[TwoToken.D]("d")

    val ss = NonTerminal[String]("S")
    val cc = NonTerminal[String]("CC")

    override val eof = Terminal[TwoToken.EOF]
    override val error = Terminal[TwoToken.ERROR]
    override val start = ss

    (ss ::= (cc, cc)) { _ => "s" }
    (cc ::= (c, cc)) { _ => "c1" }
    (cc ::= d) { _ => "d" }


  test("first") {
    val lr1 = LR1ParserGenerator(Example)
    assertEquals(lr1.first(Example.ss), Set(Example.c, Example.d))
    assertEquals(lr1.first(Example.cc), Set(Example.c, Example.d))
  }

  test("lr1 closure and start state") {
    val lr1 = LR1ParserGenerator(Example)

    val ag = lr1.augmentedRule
    val sr = Example.rules.filter(_.left == Example.ss).head
    val c1 = Example.rules.filter(r => r.left == Example.cc && r.right.size == 2).head
    val cd = Example.rules.filter(r => r.left == Example.cc && r.right.size == 1).head

    val sstate = Set(ag.toItem(Example.eof), sr.toItem(Example.eof),
                     c1.toItem(Example.c), c1.toItem(Example.d),
                     cd.toItem(Example.c), cd.toItem(Example.d))
    assertEquals(lr1.startState, sstate)
  }
}
