package pudu.parser

/** Data needed to construct a Parser. It is serializable, which could be used to
 *  save the specification to a file */
abstract class ParserSpec[Tree, Token <: scala.reflect.Enum] extends Serializable
