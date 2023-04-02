package pudu.parser

/** Base class for parsers */
abstract class Parser[Tree, Token <: scala.reflect.Enum](spec: ParserSpec[Tree, Token]):
  def parse(tokens: Iterator[Token]): Tree
