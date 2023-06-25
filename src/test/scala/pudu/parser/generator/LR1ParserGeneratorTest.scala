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
    val lr1 = LR1ParserGenerator(Example.grammar.augmented)
    assertEquals(lr1.first(Example.ss), Set(Example.c, Example.d))
    assertEquals(lr1.first(Example.cc), Set(Example.c, Example.d))
  }

  test("lr1 closure and start state") {
    val grammar = Example.grammar.augmented
    val lr1 = LR1ParserGenerator(grammar)

    val ag = grammar.startRules.head
    val sr = grammar.rules.filter(_.left == Example.ss).head // ss ::= cc cc
    val c1 = grammar.rules.filter(r => r.left == Example.cc && r.right.size == 2).head // cc ::= c cc
    val cd = grammar.rules.filter(r => r.left == Example.cc && r.right.size == 1).head // cc ::= d

    val sstate = Set(ag.toItem(Example.eof), sr.toItem(Example.eof),
                     c1.toItem(Example.c), c1.toItem(Example.d),
                     cd.toItem(Example.c), cd.toItem(Example.d))
    assertEquals(lr1.startState, sstate)
  }
}
