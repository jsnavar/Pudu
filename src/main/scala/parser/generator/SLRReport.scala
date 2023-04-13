package pudu.parser.generator

import pudu.parser._
import pudu.grammar._

/** Generates a human readable report on parser */
class SLRReport[Tree, Token <: reflect.Enum](parser: SLRParserGenerator[Tree, Token]):

  def rules: String = ???

  def states: String = ???

  def first: String = ???

  def follow: String = ???

  def lr0Automaton: String = ???

  def actions: String = ???

  def goto: String = ???
