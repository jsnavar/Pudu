package pudu.parser

import pudu.grammar.LanguageSpec

/** Subclasses of ParserGenerator should implement parser generators :), e.g. LR1ParserGenerator, LALRParserGenerator, ...
 *  Method generate returns a [[ParserSpec]], which contains all the necessary data for the parser. For example
 *  the ParserSpec for an LR1ParserGenerator could contain parsing tables and productions (needed for the reduction functions) */
abstract class ParserGenerator:
  def generate[Tree, Token <: scala.reflect.Enum](lang: LanguageSpec[Tree, Token]): ParserSpec[Tree, Token]
