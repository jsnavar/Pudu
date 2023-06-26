package pudu.parser

import pudu.grammar.LanguageSpec

/** Subclasses of ParserGenerator should implement parser generators :) */
abstract class ParserGenerator[Tree, Token <: scala.reflect.Enum]:
  def parser: Iterator[Token] => Either[ErrorMsg, Tree]
