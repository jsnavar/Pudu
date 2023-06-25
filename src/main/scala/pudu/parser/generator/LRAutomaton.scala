package pudu.parser.generator

import pudu.grammar.Symbol

class LRAutomaton[Tree, Token <: reflect.Enum](startState: State[Tree, Token], closure: State[Tree, Token] => State[Tree, Token]):
  type StateT = State[Tree, Token]

  /** goto(state, symbol) follows the standard definition */
  def goto(state: StateT, symbol: Symbol): StateT =
    // Take the items from 'state', where 'symbol' appears immediately after the dot
    val nextKernel = state.filterNot(_.after.isEmpty).filter(_.after.head == symbol)
    // Shift those items
    val shifted = nextKernel.map(_.shift)
    // And compute the closure
    closure(shifted)

  /** computes the goto of every relevant symbol in 'state', returning a map such that
   *  goto(state)(symbol) equals goto(state, symbol) */
  def goto(state: StateT): Map[(StateT, Symbol), StateT] =
    state.filterNot(_.after.isEmpty)
      .groupBy(_.after.head)
      .map((symbol, gotoState) => ((state, symbol), closure(gotoState.map(_.shift))))

  /** Computes the LR automaton, i.e. a map that given a state and a symbol, returns
   *  the next state */
  lazy val lrAutomaton: Map[(StateT, Symbol), StateT] =
    def computeAutomaton(current: Map[(StateT, Symbol), StateT], computed: Set[StateT], frontier: Set[StateT]): Map[(StateT, Symbol), StateT] =
      // Compute goto for each state in frontier, getting a Map[(StateT, Symbol), StateT] with all the results
      val newEdges = frontier.flatMap(goto)
      // Finds all states reached in the previous step, that had not been visited before
      val newStates = newEdges.map(_._2) -- computed
      // Continue until no new state is found
      if newStates.isEmpty then current ++ newEdges
      else
        computeAutomaton(current ++ newEdges, computed ++ frontier, newStates)
    computeAutomaton(Map.empty, Set.empty, Set(startState))

  lazy val indexedStates =
    val nonStartingStates = lrAutomaton.values.toSet
    val states = nonStartingStates + startState
    val numOfStates = states.size
    nonStartingStates.zip(1 until numOfStates).toMap + (startState -> 0)
