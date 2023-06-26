package pudu.grammar

case class UndefinedNonTerminalException(nonTerminals: Set[Symbol]) extends Exception:
  override def getMessage() =
    val s = if nonTerminals.size > 1 then "s" else ""
    s"Missing productions for non terminal$s <${nonTerminals.mkString(", ")}>"
