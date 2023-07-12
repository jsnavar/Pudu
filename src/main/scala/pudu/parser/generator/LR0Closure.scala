package pudu.parser.generator

import scala.collection.mutable.HashSet

import pudu.grammar.{Symbol, Rule}

class LR0Closure[Tree, Token <: reflect.Enum](rules: Set[Rule[Tree, Token]]):
  type StateT = State[Tree, Token]

  private val nonTerminals = rules.map(_.left)

  private val groupedItems = rules.map(_.toItem).groupBy(_.left)
  private val ntEdges = rules.filter(r => nonTerminals(r.right.head))
                          .map(r => r.left -> r.right.head)
                          .groupMap(_._1)(_._2)
                          .withDefaultValue(Set.empty)

  def closure(state: StateT): StateT =
    val visited = HashSet.empty[Symbol]

    def closureImpl(frontier: Set[Symbol]): StateT =
      val newNonTerminals = frontier.flatMap(ntEdges.andThen(_.filterNot(visited.contains)))
      if newNonTerminals.isEmpty then
        (visited.flatMap(groupedItems) ++= state).toSet
      else
        visited ++= newNonTerminals
        closureImpl(newNonTerminals)
    val start = state.filter(item => !item.after.isEmpty && nonTerminals(item.after.head)).map(_.after.head)
    visited ++= start
    closureImpl(start)
