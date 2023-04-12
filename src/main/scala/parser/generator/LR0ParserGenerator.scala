package pudu.parser.generator

import pudu.parser._
import pudu.grammar._

class LR0ParserGenerator[Tree, Token <: reflect.Enum](lang: LanguageSpec[Tree, Token]) extends LRParserGenerator(lang):
  def parser: Iterator[Token]=>Either[ErrorMsg, Tree] = ???
