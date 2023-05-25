package pudu.parser.generator

import pudu.grammar._

class ReduceReduceConflictException[T,ST](conflict: Iterable[Rule[T,ST]]) extends Exception:
  override def getMessage() =
      "RR conflict:\n" + conflict.map(rule => s"\t$rule").mkString("\n")

class ShiftReduceConflictException[T,ST](rule: Rule[T,ST], symbol: Symbol) extends Exception:
  override def getMessage() =
    s"SR conflict:\n\tTerminal: $symbol\nRule:$rule"
