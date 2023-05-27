package pudu.parser.generator

import pudu.grammar._

class ReduceReduceConflictException[Tree,Token](conflict: Iterable[Rule[Tree,Token]]) extends Exception:
  override def getMessage() =
      "RR conflict:\n" + conflict.map(rule => s"\t$rule").mkString("\n")

class ShiftReduceConflictException[Tree,Token](rule: Rule[Tree,Token], symbol: Symbol) extends Exception:
  override def getMessage() =
    s"SR conflict:\n\tTerminal: $symbol\nRule:$rule"
