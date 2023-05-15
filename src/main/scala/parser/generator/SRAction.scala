package pudu.parser.generator

import pudu.grammar.Rule

/** Shift-reduce actions. */
abstract class SRAction

/** Shift to 'state' */
case class Shift(state: Int) extends SRAction

/** Reduce by 'rule' */
case class Reduce[T, ST](rule: Rule[T,ST]) extends SRAction

/** Accept (no parameters) */
object Accept extends SRAction

/** Error (no parameters) */
object Error extends SRAction


