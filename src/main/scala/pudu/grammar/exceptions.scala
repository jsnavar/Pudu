package pudu.grammar

case class UndefinedNonTerminalException(nonTerminals: Set[Symbol]) extends Exception:
  override def getMessage() =
    if nonTerminals.size == 1 then s"Missing productions for non terminal ${nonTerminals.head}"
    else
      val ntStr = nonTerminals.mkString(", ")
      s"Non terminals { $ntStr } were used without productions"
