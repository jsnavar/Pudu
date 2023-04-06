package pudu.grammar

/** Grammar rules/productions */

case class Rule[Tree, ST](left: NonTerminal[Tree], right: Seq[Symbol], action: Seq[ST]=>ST):
  val arity = right.size
  def reduce(stack: Seq[ST]): Seq[ST] =
    val (top, bot) = stack.splitAt(arity)
    val result = action(top)
    result +: bot
