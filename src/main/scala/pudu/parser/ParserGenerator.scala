package pudu.parser

import pudu.grammar.LanguageSpec

/** Subclasses of ParserGenerator should implement parser generators :), e.g. LR1ParserGenerator, LALRParserGenerator, ...
 *  a parser is a function of type Iterator[Token] => Either[ErrorMsg, Tree] */
abstract class ParserGenerator[Tree, Token <: scala.reflect.Enum]:
  def parser: Iterator[Token] => Either[ErrorMsg, Tree]
