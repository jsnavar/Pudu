package pudu.parser.generator

import pudu.grammar._
import pudu.parser._

class SLRParserGenerator[Tree, Token <: scala.reflect.Enum](grammar: Grammar[Tree,Token]) extends LRParserGenerator(grammar):

  /** For each state 'state', compute pairs ((state, terminal), rule),
   *  where rule is the rule to reduce by given the pair (state, terminal).
   *  Throws a ReduceReduceConflictException in case of a RR conflict. */
  override lazy val reduceActions: Map[(StateT, Terminal[Token]), Set[RuleT]] =
    /* First, we generate tuples (state, terminal, rule), such that there exists
     * an item X -> \alpha\cdot in state, and terminal\in FOLLOW(X). */
    val reduceByCases = for
      state <- indexedStates.keys
      item <- state
      if item.after.isEmpty
      terminal <- follow(item.left)
    yield (state, terminal, item.rule)
    /* Then, we group that tuples into ((state, terminal), rule), ensuring
     * that only tuple exists for each pair (state, terminal) */
    reduceByCases.groupMap(t => (t._1, t._2))(_._3).view
      .mapValues(_.toSet).toMap
