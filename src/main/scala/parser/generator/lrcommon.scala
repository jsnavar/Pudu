package pudu.parser.generator

import pudu.grammar._

/** Items are rules with a dot somewhere in the right side. This is represented with two Seq, for before
 *  and after the dot */
case class Item[T,ST](left: NonTerminal[T], before: Seq[Symbol], after: Seq[Symbol], rule: Rule[T,ST]):
  /** shifts the dot one place to the right. Caller must ensure that 'after' is not empty before calling */
  def shift = Item(left, before :+ after.head, after.tail, rule)

extension[T,ST] (rule: Rule[T,ST])
  def toItem: Item[T,ST] = Item(rule.left, Seq.empty[Symbol], rule.right, rule)

trait LRParserGenerator[Tree, Token <: scala.reflect.Enum]:
  type RuleT = Rule[Tree, Tree|Token]
  type ItemT = Item[Tree, Tree|Token]
  /** A state is just a set of Items. This may be changed in the future */
  type State = Set[ItemT]

  /** Closure of a state. Follows the standard definition */
  def closure(rules: Set[RuleT])(state: State): State =
    stateClosure(state, rules.map(_.toItem))

  def stateClosure(state: State, candidates: Set[ItemT]): State =
    val starting = state.filterNot(_.after.isEmpty).map(_.after.head)
    val newItems = candidates.filter((item: ItemT) => starting.contains(item.left))
    if newItems.isEmpty then state
    else stateClosure(state ++ newItems, candidates -- newItems)
