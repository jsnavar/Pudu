package pudu.grammar

/** Grammar rules/productions */

case class Rule[Tree, Token](left: NonTerminal[Tree], right: Seq[Symbol], action: Seq[Tree|Token]=>(Tree|Token)):
  type ST = Tree | Token
  val arity = right.size
  def reduce(stack: Seq[ST]): Seq[ST] =
    val (top, bot) = stack.splitAt(arity)
    val result = action(top)
    result +: bot

  override def toString =
    s"$left ::= ${right.mkString(" ")}"
