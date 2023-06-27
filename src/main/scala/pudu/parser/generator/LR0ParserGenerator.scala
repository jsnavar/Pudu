package pudu.parser.generator

import pudu.parser._
import pudu.grammar._

/** Exercise for the reader :). */
class LR0ParserGenerator[Tree, Token <: reflect.Enum](grammar: Grammar[Tree, Token]) extends ParserGenerator[Tree, Token]:
  def parser = ???
