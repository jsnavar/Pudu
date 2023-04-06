package pudu.parser.generator

import pudu.grammar._
import pudu.parser._

/** Items are rules with a dot somewhere in the right side. This is represented with two Seq, for before
 *  and after the dot */
case class Item[T,ST](left: NonTerminal[T], before: Seq[Symbol], after: Seq[Symbol], rule: Rule[T,ST]):
  /** shifts the dot one place to the right. Caller must ensure that 'after' is not empty before calling */
  def shift = Item(left, before :+ after.head, after.tail, rule)

extension[T,ST] (rule: Rule[T,ST])
  def toItem: Item[T,ST] = Item(rule.left, Seq.empty[Symbol], rule.right, rule)

abstract class LRParserGenerator[Tree, Token <: scala.reflect.Enum](lang: LanguageSpec[Tree,Token]) extends ParserGenerator[Tree, Token]:
  type RuleT = Rule[Tree, Tree|Token]
  type ItemT = Item[Tree, Tree|Token]
  /** A state is just a set of Items. This may be changed in the future */
  type State = Set[ItemT]

  val rules = lang.rules
  val terminals = lang.terminals
  val nonTerminals = lang.nonTerminals
  val symbols = terminals ++ nonTerminals
  val precedence = lang.precedence

  /** Closure of a state. Follows the standard definition */
  def closure(state: State): State =
    stateClosure(state, rules.map(_.toItem))

  def stateClosure(state: State, candidates: Set[ItemT]): State =
    val starting = state.filterNot(_.after.isEmpty).map(_.after.head)
    val newItems = candidates.filter((item: ItemT) => starting.contains(item.left))
    if newItems.isEmpty then state
    else stateClosure(state ++ newItems, candidates -- newItems)
