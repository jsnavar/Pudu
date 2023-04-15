package pudu.parser.generator

import pudu.grammar._

class ReduceReduceConflictException[T,ST](conflicts: Iterable[Iterable[Rule[T,ST]]]) extends Exception:
  override def getMessage() =
    conflicts.map{ iterable =>
      "RR conflict:\n" + iterable.map(rule => s"\t$rule").mkString("\n")
    }.mkString("\n\n")

class ShiftReduceConflictException[T,ST](rule: Rule[T,ST], symbol: Symbol) extends Exception:
  override def getMessage() =
    s"SR conflict:\n\tTerminal: $symbol\nRule:$rule"
