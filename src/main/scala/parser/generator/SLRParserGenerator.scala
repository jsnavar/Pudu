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

  /** if [A -> \alpha\cdot] is in state, and terminal is in Follow(A), then
    * reduce by the rule A -> \alpha */
  lazy val reduceActions: Map[(State, Terminal[Token]), RuleT] =
    /** Given an item A -> \alpha\cdot in state, returns a tuple (state, s, A -> \alpha)
     *  for each s\in follow(A) */
    def itemReduceActions(state: State, item: ItemT): Set[(State, Terminal[Token], RuleT)] =
      require(item.after.isEmpty)
      follow(item.left).map(symbol => (state, symbol, item.rule))

    states.flatMap { state =>
      /* For each state 'state', compute pairs ((state, symbol), rules),
       * where rules is the set of rules to reduce by, given the pair (state, symbol). */
      val stateResult = state.filter(_.after.isEmpty).flatMap(itemReduceActions(state,_))
        .groupMap(t => (t._1, t._2))(_._3)

      /* If for some value of (state, symbol), stateResult((state, symbol)) contains more
       * than one element, then we have a RR conflict */
      if stateResult.find(_._2.size != 1).isDefined then
        throw ReduceReduceConflictException(stateResult.find(_._2.size != 1).map(_._2))
      else
        stateResult
    }.toMap.mapValues(_.head).toMap // Finally, as all sets of rules are singletons, get the first element from each

  /** Decides the action for an automaton edge */
  def edgeAction(from: (State, Terminal[Token]), to: State): ActionTableEntry =
    val (state, terminal) = from
    val reduce = reduceActions.contains(state, terminal)
    // if [A -> \alpha\cdot a\beta] is in state, and a = terminal, then
    // shift to 'to'
    val shift = state.find((item: ItemT) => !item.after.isEmpty && item.after.head == terminal).isDefined
    if shift && reduce then
      // SR Conflict
      shiftReduceResolution(state, to)(reduceActions(state, terminal), terminal)
    else if shift then shiftTo(state, terminal, to) 
    else if reduce then reduceBy(state, terminal, reduceActions(state, terminal))
    else errorOn(state, terminal) // This should never happen

  // ((stateIndex, nonTerminalSymbol), stateIndex)
  type GotoTableEntry = ((Int, Symbol), Int)

  def edgeGoto(from: (State, Symbol), to: State): GotoTableEntry =
    val fromState = from._1
    val fromStateIdx = indexedStates(fromState)
    val symbol = from._2
    val toIdx = indexedStates(to)
    (fromStateIdx, symbol) -> toIdx

  def parser: Iterator[Token] => Either[ErrorMsg, Tree] =
    // split automaton edges in terminal and nonterminal edges
    val splittedEdges = lr0Automaton.groupBy((key, value) => isTerminal(key._2))
    val nonTerminalEdges = splittedEdges(false)

    val terminalEdges = splittedEdges(true)
    // cast symbols to 'Terminal[Token]'.
    val castedTerminalEdges = terminalEdges.map((key, value) =>
      val (keyState, keySymbol) = key
      if keySymbol.isInstanceOf[Terminal[_]] then
        ((keyState, keySymbol.asInstanceOf[Terminal[Token]]), value)
      else
        throw Exception("There was a non terminal in 'terminals'"))

    // accept state:
    val startState = closure(Set(augmentedRule.toItem))
    val acceptStateIndex = indexedStates(lr0Automaton(startState, lang.start))

    val reduce = reduceActions.map((k, rule) => reduceBy(k._1, k._2, rule))

    // build action and goto tables, using the functions defined above, and
    // add acceptance condition to actionTable
    val actionTable = (reduce ++ castedTerminalEdges.map(edgeAction)).updated((acceptStateIndex, eof.ordinal), Accept)
    val gotoTable = nonTerminalEdges.map(edgeGoto)
    lrParse(actionTable, gotoTable)
