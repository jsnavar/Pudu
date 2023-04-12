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

  def edgeAction(from: (State, Terminal[Any]), to: State): ActionTableEntry =
    val (state, terminal) = from
    // if [A -> \alpha\cdot] is in state, and terminal is in Follow(A), then
    // reduce by the rule A -> \alpha
    val reduceBy = state.filter((item: ItemT) => item.after.isEmpty
                                && item.left != startSymbol
                                && follow(item.left).contains(terminal))
    if reduceBy.size > 1 then throw ReduceReduceConflictException()
    // if [A -> \alpha\cdot a\beta] is in state, and a = terminal, then
    // shift to to
    val shift = state.find(_.after.head == terminal).isDefined
    if shift && reduceBy.size != 0 then
      throw ShiftReduceConflictException()
    else if shift then
      ((indexedStates(state), terminal.ordinal), Shift(indexedStates(to)))
    else
      ((indexedStates(state), terminal.ordinal), Reduce(reduceBy.head.rule))


  // ((stateIndex, nonTerminalSymbol), stateIndex)
  type GotoTableEntry = ((Int, Symbol), Int)

  def edgeGoto(from: (State, Symbol), to: State): GotoTableEntry =
    ((indexedStates(from._1), from._2), indexedStates(to))

  def parser: Iterator[Token] => Tree =
    // split automaton edges in terminal and nonterminal edges
    val splittedEdges = lr0Automaton.groupBy((key, value) => isTerminal(key._2))
    val nonTerminalEdges = splittedEdges(false)

    val terminalEdges = splittedEdges(true)
    // 'cast' symbols to 'Terminal[Any]'.
    val castedTerminalEdges = terminalEdges.map((key, value) => key._2 match
                                                  case terminal: Terminal[_] => ((key._1, terminal), value)
                                                  case _ => throw IllegalArgumentException("There was a non terminal in 'terminal'"))
    // accept state:
    val startState = closure(Set(augmentedRule.toItem))
    val acceptStateIndex = indexedStates(lr0Automaton(startState, lang.start))

    // build action and goto tables, using the functions defined above, and
    // add acceptance entry to actionTable
    val actionTable = castedTerminalEdges.map(edgeAction).updated((acceptStateIndex, eof.ordinal), Accept)
    val gotoTable = nonTerminalEdges.map(edgeGoto)
    lrParse(actionTable, gotoTable)
