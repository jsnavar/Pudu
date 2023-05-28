package pudu.parser.generator

import pudu.grammar._
import pudu.parser._

class SLRParserGenerator[Tree, Token <: scala.reflect.Enum](lang: LanguageSpec[Tree,Token]) extends LRParserGenerator(lang):
  val startState = closure(Set(augmentedRule.toItem))
  val nonStartingStates = lr0Automaton.values.toSet
  val states = nonStartingStates + startState
  val numOfStates = states.size
  val indexedStates = nonStartingStates.zip(1 until numOfStates).toMap + (startState -> 0)

  // ((stateIndex, tokenOrdinal), Action)
  type ActionTableEntry = ((Int, Int), SRAction)

  // ((stateIndex, nonTerminalSymbol), stateIndex)
  type GotoTableEntry = ((Int, Symbol), Int)

  /** builds the action table key */
  def actionTableKey(from: State, terminal: Terminal[Token]) =
    val fromIdx = indexedStates(from)
    val terminalOrdinal = terminal.ordinal
    (fromIdx, terminalOrdinal)

  /** Action table entry for shift actions */
  def shiftTo(from: State, terminal: Terminal[Token], to: State): ActionTableEntry =
    val key = actionTableKey(from, terminal)
    val toIdx = indexedStates(to)
    (key, Shift(toIdx))

  /** Action table entry for reduce actions */
  def reduceBy(from: State, terminal: Terminal[Token], rule: RuleT): ActionTableEntry =
    val key = actionTableKey(from, terminal)
    (key, Reduce(rule))

  /** Action table entry for accept action */
  def acceptOn(acceptState: State): ActionTableEntry =
    val key = actionTableKey(acceptState, eof)
    (key, Accept)

  /** uses precedence to solve shift reduce conflicts. Default is to shift, in
   *  accordance to tradition (https://www.gnu.org/software/bison/manual/html_node/How-Precedence.html) */
  def shiftReduceResolution(from: State, to: State)(rule: RuleT, terminal: Terminal[Token]): ActionTableEntry =
    val lastTerminalOfRule = rule.right.findLast(isTerminal)
    if !lastTerminalOfRule.isDefined then
      // shift by default
      shiftTo(from, terminal, to)
    else precedence.max(lastTerminalOfRule.get, terminal) match
      case Side.Left => reduceBy(from, terminal, rule)
      case Side.Right => shiftTo(from, terminal, to)
      case Side.Neither => shiftTo(from, terminal, to) // shift by default
      case Side.Error => throw ShiftReduceConflictException(rule, terminal)

  /** For each state 'state', compute pairs ((state, terminal), rule),
   *  where rule is the rule to reduce by given the pair (state, terminal).
   *  Throws a ReduceReduceConflictException in case of a RR conflict. */
  val reduceActions: Map[(State, Terminal[Token]), RuleT] =
    /* First, we generate tuples (state, terminal, rule), such that there exists
     * an item X -> \alpha\cdot in state, and terminal\in FOLLOW(X). */
    val reduceByCases = for
      state <- states
      item <- state
      if item.after.isEmpty
      terminal <- follow(item.left)
    yield (state, terminal, item.rule)
    /* Then, we group that tuples into ((state, terminal), rule), ensuring
     * that only tuple exists for each pair (state, terminal) */
    reduceByCases.groupMap(t => (t._1, t._2))(_._3).view
      .mapValues { rules =>
        if rules.size > 1 then throw ReduceReduceConflictException(rules)
        rules.head
      }.toMap

  val (actionTable, gotoTable) =
    type LRTables = (ActionTable, GotoTable)

    /* Update LRTables for a given LR0 automaton edge */
    def updateTables(tables: LRTables, edge: ((State, Symbol), State)): LRTables =
      /* unapply edge and tables to simplify code */
      val ((fromState, symbol), toState) = edge
      val (actionsTable, gotoTable) = tables

      /* updates will depend on the type of symbol */
      symbol match
        case t: Terminal[_] =>
          val terminal = t.asInstanceOf[Terminal[Token]]
          /* This assert is true by construction of the LR0 automaton */
          assert(fromState.exists(item => !item.after.isEmpty && item.after.head == terminal))
          /* Check if there is a SR conflict */
          val action = if reduceActions.contains(fromState, terminal) then
            val reduceByRule = reduceActions(fromState, terminal)
            shiftReduceResolution(fromState, toState)(reduceByRule, terminal)
          else shiftTo(fromState, terminal, toState)
          /* update actionsTable */
          (actionsTable + action, gotoTable)
        case _: NonTerminal[_] =>
          /* For non terminals, the edge corresponds to an entry in the goto table */
          val fromStateIdx = indexedStates(fromState)
          val toStateIdx = indexedStates(toState)
          val gotoEntry = (fromStateIdx, symbol) -> toStateIdx
          (actionsTable, gotoTable + gotoEntry)

    // Transform the Map reduceAction into an initial ActionTable
    val reduce = reduceActions.map { case ((from, terminal), rule) => reduceBy(from, terminal, rule) }
    // Compute tables. Actions table is partial because it lacks the accept condition */
    val (partialActionTable, gotoTable) = lr0Automaton.foldLeft((reduce, Map.empty))(updateTables)

    // add accept condition
    val acceptState = lr0Automaton(startState, lang.start)
    val acceptEntry: ActionTableEntry = acceptOn(acceptState)

    (partialActionTable + acceptEntry, gotoTable)

  def parser: Iterator[Token] => Either[ErrorMsg, Tree] =
    lrParse(actionTable, gotoTable)
