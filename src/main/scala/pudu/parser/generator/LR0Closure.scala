package pudu.parser.generator

import pudu.grammar.Rule

class LR0Closure[Tree, Token <: reflect.Enum](rules: Set[Rule[Tree, Token]]):
  type StateT = State[Tree, Token]
  def closure(state: StateT): StateT =
    def stateClosure(state: StateT, candidates: Set[LRItem[Tree, Token]]): StateT =
      // First, we take the symbols after the dot for each item in 'state'
      val starting = state.filterNot(_.after.isEmpty).map(_.after.head)
      // Then, select the candidates whose left part is in the 'starting' set
      val newItems = candidates.filter(item => starting.contains(item.left))
      // Continue recursively until no more items are added
      if newItems.isEmpty then state
      else stateClosure(state ++ newItems, candidates -- newItems)
    stateClosure(state, rules.map(_.toItem))
