package pudu.parser.generator

import pudu.grammar.{Symbol, Rule}

class LR0Closure[Tree, Token <: reflect.Enum](rules: Set[Rule[Tree, Token]]):
  type StateT = State[Tree, Token]

  private val nonTerminals = rules.map(_.left)

  private val groupedItems = rules.map(_.toItem).groupBy(_.left)
  private val nextNT = rules.filter(r => nonTerminals(r.right.head))
                          .groupMap(_.left)(_.right.head)
                          .withDefaultValue(Set.empty)

  def closure(state: StateT): StateT =
    def closureImpl(visited: Set[Symbol], frontier: Set[Symbol]): StateT =
      val newNonTerminals = frontier.flatMap(nextNT.andThen(_.filterNot(visited)))
      if newNonTerminals.isEmpty then
        visited.flatMap(groupedItems) ++ state
      else
        closureImpl(visited ++ newNonTerminals, newNonTerminals)

    val start = state.filter(item => !item.after.isEmpty && nonTerminals(item.after.head)).map(_.after.head)
    closureImpl(start, start)
