package pudu.parser.generator

class UnresolvedConflictException(reportPath: String) extends Exception:
  override def getMessage() = (s"Unresolved conflicts. Report written to '${reportPath}'.")
