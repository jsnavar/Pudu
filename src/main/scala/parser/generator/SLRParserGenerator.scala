package pudu.parser.generator

import pudu.grammar._
import pudu.parser._

class SLRParserGenerator[Tree, Token <: scala.reflect.Enum](lang: LanguageSpec[Tree,Token]) extends LRParserGenerator(lang):
  def parser: Iterator[Token] => Tree = ???
