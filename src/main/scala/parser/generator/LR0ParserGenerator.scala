package pudu.parser.generator

import pudu.parser.ParserGenerator
import pudu.grammar._

class LR0ParserGenerator[Tree, Token <: reflect.Enum] extends ParserGenerator[Tree, Token]:
  def apply(lang: LanguageSpec[Tree, Token]): Iterator[Token] => Tree = ???
