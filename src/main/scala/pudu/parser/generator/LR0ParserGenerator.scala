package pudu.parser.generator

import pudu.parser._
import pudu.grammar._

/** Exercise for the reader :). It should be easy, as most of the work is already done in 'LRParserGenerator' */
class LR0ParserGenerator[Tree, Token <: reflect.Enum](grammar: Grammar[Tree, Token]) extends LRParserGenerator(grammar):
  override lazy val reduceActions: Map[(StateT, Terminal[Token]), Set[RuleT]] = ???
