package pudu.parser.generator

import scala.collection.mutable.{AnyRefMap, LinkedHashSet, HashSet}

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
   *  (goto(state))(symbol) equals goto(state, symbol) */
  def goto(state: StateT): Map[Symbol, StateT] =
    state.filterNot(_.after.isEmpty)
      .groupMap(_.after.head)(_.shift)
      .transform((_,v) => closure(v))

  lazy val lrAutomaton: Map[(StateT, Symbol), StateT] =
    val current = AnyRefMap.empty[(StateT, Symbol), StateT]
    val visited = HashSet(startState)
    val newStates = HashSet.empty[StateT]

    def computeAutomaton(frontier: Set[StateT]): Map[(StateT, Symbol), StateT] =
      newStates.clear()
      for
        state <- frontier
        case (sym, to) <- goto(state)
      do
        current += (state, sym) -> to
        if !visited.contains(to) then
          newStates += to
          visited += to

      if newStates.isEmpty then current.toMap
      else computeAutomaton(newStates.toSet)
    computeAutomaton(Set(startState))

  lazy val indexedStates = (LinkedHashSet(startState) ++= lrAutomaton.values).zipWithIndex.toMap
