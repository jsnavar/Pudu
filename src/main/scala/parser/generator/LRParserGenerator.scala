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
  val precedence = lang.precedence

  val terminals = lang.terminals
  val nonTerminals = lang.nonTerminals
  val symbols = terminals ++ nonTerminals

  def isTerminal(symbol: Symbol) = terminals.contains(symbol)
  def isNonTerminal(symbol: Symbol) = nonTerminals.contains(symbol)

  /** Closure of a state. Follows the standard definition */
  def closure(state: State): State =
    stateClosure(state, rules.map(_.toItem))

  def stateClosure(state: State, candidates: Set[ItemT]): State =
    val starting = state.filterNot(_.after.isEmpty).map(_.after.head)
    val newItems = candidates.filter((item: ItemT) => starting.contains(item.left))
    if newItems.isEmpty then state
    else stateClosure(state ++ newItems, candidates -- newItems)

  /** goto(state, symbol) follows the standard definition */
  def goto(state: State, symbol: Symbol): State =
    val nextKernel = state.filterNot(_.after.isEmpty).filter(_.after.head == symbol)
    val shifted = nextKernel.map(_.shift)
    closure(shifted)

  /** computes the goto of every relevant symbol in 'state', returning a map such that
   *  goto(state)(symbol) := goto(state, symbol) */
  def goto(state: State): Map[Symbol, State] =
    state.filterNot(_.after.isEmpty)
      .groupBy(_.after.head)
      .map((symbol, state) => (symbol, closure(state.map(_.shift))))

  /** Pudu does not support null productions, so first reduces to reachability.
   *  We compute that using a fixed point */
  def first: Map[Symbol, Set[Symbol]] =
    // we only need the first symbol in each rule:
    val edges = rules.map(rule => rule.left -> rule.right.head)
    def firstFP(current: Set[(Symbol, Symbol)]): Set[(Symbol, Symbol)] =
      //compute the new pairs, using a for expression
      val next = for
        (el, er) <- edges
        (pl, pr) <- current
        if pl == er
        if !current.contains((el, pr))
      yield (el, pr)
      if next.isEmpty then current
      else firstFP(current ++ next)
    val start = terminals.map(t => t -> t)
    /* firstFP(start) contains pairs (N, T), such that terminal T belongs
     * to first(N) */
    firstFP(start).groupBy(_._1)
      .map((l, r) => l -> r.map(_._2))

  def follow(symbol: Symbol): Set[Symbol] = ???
