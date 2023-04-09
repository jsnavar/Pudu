package pudu.parser.generator

import pudu.grammar.Rule

abstract class SRAction
case class Shift(state: Int) extends SRAction
case class Reduce[T, ST](rule: Rule[T,ST]) extends SRAction
object Accept extends SRAction
object Error extends SRAction


