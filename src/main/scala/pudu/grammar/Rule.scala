package pudu.grammar

/** Grammar rules/productions */

case class Rule[Tree, Token](left: NonTerminal[Tree], right: Seq[Symbol], action: Seq[Tree|Token]=>Tree):
  val arity = right.size

  override def toString =
    s"$left ::= ${right.mkString(" ")}"
