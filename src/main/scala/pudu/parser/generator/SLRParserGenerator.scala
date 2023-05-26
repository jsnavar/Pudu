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

  def errorOn(from: State, terminal: Terminal[Token]): ActionTableEntry =
    val key = actionTableKey(from, terminal)
    (key, Error)

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
  lazy val reduceActions: Map[(State, Terminal[Token]), RuleT] =
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
    reduceByCases.groupMap(t => (t._1, t._2))(_._3)
      .mapValues { rules =>
        if rules.size > 1 then throw ReduceReduceConflictException(rules)
        rules.head
      }.toMap

  /** Decides the action for a pair (state, terminal), returning an ActionTableEntry */
  def edgeAction(from: (State, Terminal[Token]), to: State): ActionTableEntry =
    val (state, terminal) = from
    val reduce = reduceActions.contains(state, terminal)
    // if [A -> \alpha\cdot a\beta] is in state, and a equals to 'terminal', then
    // shift to 'to'
    val shift = state.exists(item => !item.after.isEmpty && item.after.head == terminal)
    if shift && reduce then
      // SR Conflict
      shiftReduceResolution(state, to)(reduceActions(state, terminal), terminal)
    else if shift then shiftTo(state, terminal, to)
    else if reduce then reduceBy(state, terminal, reduceActions(state, terminal))
    else errorOn(state, terminal) // This should never happen

  // ((stateIndex, nonTerminalSymbol), stateIndex)
  type GotoTableEntry = ((Int, Symbol), Int)

  def edgeGoto(from: (State, Symbol), to: State): GotoTableEntry =
    val (fromState, symbol) = from
    val fromStateIdx = indexedStates(fromState)
    val toIdx = indexedStates(to)
    (fromStateIdx, symbol) -> toIdx

  lazy val (actionTable, gotoTable) =
    // split automaton edges in terminal and nonterminal edges
    val splittedEdges = lr0Automaton.groupBy((key, value) => isTerminal(key._2))

    val nonTerminalEdges = splittedEdges(false)
    val terminalEdges = splittedEdges(true)

    // cast terminal symbols to 'Terminal[Token]'.
    val castedTerminalEdges = terminalEdges.map((key, value) =>
      val (keyState, keySymbol) = key
      if keySymbol.isInstanceOf[Terminal[_]] then
        ((keyState, keySymbol.asInstanceOf[Terminal[Token]]), value)
      else
        throw Exception("There was a non terminal in 'terminals'"))

    // Transform the Map reduceAction into ActionTableEntries */
    val reduce = reduceActions.map((k, rule) => reduceBy(k._1, k._2, rule))

    // build action and goto tables using the functions defined above
    val actionTable = reduce ++ castedTerminalEdges.map(edgeAction)
    val gotoTable = nonTerminalEdges.map(edgeGoto)

    // accept state
    val startState = closure(Set(augmentedRule.toItem))
    val acceptState = lr0Automaton(startState, lang.start)
    val acceptEntry: ActionTableEntry = acceptOn(acceptState)

    (actionTable + acceptEntry, gotoTable)

  def parser: Iterator[Token] => Either[ErrorMsg, Tree] =
    lrParse(actionTable, gotoTable)
