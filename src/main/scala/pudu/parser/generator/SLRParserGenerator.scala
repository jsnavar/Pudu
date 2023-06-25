package pudu.parser.generator

import pudu.grammar._
import pudu.parser._

class SLRParserGenerator[Tree, Token <: scala.reflect.Enum](grammar: Grammar[Tree,Token]) extends ParserGenerator[Tree, Token]:
  type StateT = State[Tree, Token]

  val augmentedGrammar = grammar.augmented
  val lrff = LRFirstFollow(augmentedGrammar)

  val closure = LR0Closure(augmentedGrammar.rules).closure

  val startState = closure(augmentedGrammar.startRules.map(_.toItem))
  val lra = LRAutomaton(startState, closure)

  /** For each state 'state', compute pairs ((state, terminal), rule),
   *  where rule is the rule to reduce by given the pair (state, terminal).
   *  Throws a ReduceReduceConflictException in case of a RR conflict. */
  val reduceActions: Map[(StateT, Terminal[Token]), Set[Rule[Tree, Token]]] =
    /* First, we generate tuples (state, terminal, rule), such that there exists
     * an item X -> \alpha\cdot in state, and terminal\in FOLLOW(X). */
    val reduceByCases = for
      state <- lra.indexedStates.keys
      item <- state
      if item.after.isEmpty
      terminal <- lrff.follow(item.left)
    yield (state, terminal, item.rule)
    /* Then, we group that tuples into ((state, terminal), rule), ensuring
     * that only tuple exists for each pair (state, terminal) */
    reduceByCases.groupMap(t => (t._1, t._2))(_._3).view
      .mapValues(_.toSet).toMap

  val parserGen = LRParserGenerator(augmentedGrammar, lra.lrAutomaton, lra.indexedStates, reduceActions)

  def report = LRReport(augmentedGrammar.rules,
                        augmentedGrammar.terminalNames,
                        lrff.first,
                        lrff.follow,
                        lra.indexedStates,
                        lra.lrAutomaton,
                        parserGen.actionTable,
                        parserGen.gotoTable)

  def parser = parserGen.parser
