package pudu.parser.generator

import pudu.grammar._
import pudu.parser._

class LR1ParserGenerator[Tree, Token <: scala.reflect.Enum](lang: LanguageSpec[Tree,Token]) extends LRParserGenerator(lang):

  val startState = closure(Set(augmentedRule.toItem(Set(eof))))

  def parser: Iterator[Token]=>Either[ErrorMsg, Tree] = ???
