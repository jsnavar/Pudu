package pudu.parser.generator

import pudu.grammar.Rule

/** Shift-reduce actions. */
sealed trait SRAction

object SRAction:
  /** Shift to 'state' */
  case class Shift(state: Int) extends SRAction

  /** Reduce by 'rule' */
  case class Reduce[Tree, Token](rule: Rule[Tree,Token]) extends SRAction

  object Accept extends SRAction:
    override def toString = "Accept"

  object Error extends SRAction:
    override def toString = "Error"
