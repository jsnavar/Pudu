package pudu.parser.generator

import pudu.grammar._
import pudu.parser._

class LR1ParserGenerator[Tree, Token <: scala.reflect.Enum](grammar: Grammar[Tree,Token]) extends LRParserGenerator(grammar):

  import grammar._

  /** start state */
  override val startState = closure(startRules.map(_.toItem(eof)))

  /** state closure for LR1 parsers. */
  override def closure(state: StateT): StateT =
    def firstSeq(seq: Seq[Symbol], next: Option[Symbol]) =
      if !seq.isEmpty then first(seq.head)
      else if next.isDefined then first(next.get)
      else Set.empty

    def closureImpl(acc: StateT): StateT =
      val step = for
        item <- acc
        if !item.after.isEmpty && isNonTerminal(item.after.head)
        head = item.after.head
        rule <- rules
        if rule.left == head
        tok <- firstSeq(item.after.tail, item.context)
        newItem = rule.toItem(tok)
        if !acc.contains(newItem)
      yield newItem
      if step.isEmpty then acc
      else closureImpl(acc ++ step)
    closureImpl(state)

  lazy val reduceActions: Map[(StateT, Terminal[Token]), Set[RuleT]] =
    /* First, we generate tuples (state, terminal, rule), such that there exists
     * an item X -> \alpha\cdot in state, and terminal\in FOLLOW(X). */
    val reduceByCases = for
      state <- indexedStates.keys
      item <- state
      if item.after.isEmpty && item.context.isDefined
    yield (state, item.context.get, item.rule)
    /* Then, we group that tuples into ((state, terminal), rule), ensuring
     * that only tuple exists for each pair (state, terminal) */
    reduceByCases.groupMap(t => (t._1, t._2))(_._3).view
      .mapValues(_.toSet).toMap

