package pudu.parser.generator

import pudu.grammar.Rule

/** Shift-reduce actions. */
sealed trait SRAction

object SRAction:
  /** Shift to 'state' */
  case class Shift(state: Int) extends SRAction

  /** Reduce by 'rule' */
  case class Reduce[T, ST](rule: Rule[T,ST]) extends SRAction

  object Accept extends SRAction
  object Error extends SRAction


