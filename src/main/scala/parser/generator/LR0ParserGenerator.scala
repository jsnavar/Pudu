package pudu.parser.generator

import pudu.parser.ParserGenerator
import pudu.grammar._

class LR0ParserGenerator[Tree, Token <: reflect.Enum](lang: LanguageSpec[Tree, Token]) extends LRParserGenerator[Tree, Token](lang):
  def apply: Iterator[Token]=>Tree = ???

