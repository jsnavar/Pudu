package pudu.parser.generator

import pudu.grammar.Rule

class LR0Closure[Tree, Token <: reflect.Enum](rules: Set[Rule[Tree, Token]]):
  type StateT = State[Tree, Token]

  val itemSet = rules.map(_.toItem)

  def closure(state: StateT): StateT =
    def stateClosure(acc: StateT, candidates: Set[LRItem[Tree, Token]], frontier: Set[LRItem[Tree, Token]]): StateT =
      val nonTerminalsToAdd = frontier.filterNot(_.after.isEmpty).map(_.after.head)
      val newItems = candidates.filter(item => nonTerminalsToAdd.contains(item.left))
      if newItems.isEmpty then acc
      else stateClosure(acc ++ newItems, candidates -- newItems, newItems)
    stateClosure(state, itemSet, state)
