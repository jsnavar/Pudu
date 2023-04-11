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

/** Base class for LR parser generators (SLR, LR(1), LALR) */
abstract class LRParserGenerator[Tree, Token <: scala.reflect.Enum](lang: LanguageSpec[Tree,Token]) extends ParserGenerator[Tree, Token]:
  type RuleT = Rule[Tree, Tree | Token]
  type ItemT = Item[Tree, Tree | Token]
  /** A state is just a set of Items. This may be changed in the future */
  type State = Set[ItemT]

  /* Grammar is augmented with a new start symbol, as usual */
  val startSymbol = NonTerminal[Tree]
  val eof = lang.eof
  val augmentedRule: RuleT = Rule(startSymbol, Seq(lang.start), _.head)

  val rules = lang.rules + augmentedRule
  val precedence = lang.precedence

  val terminals = lang.terminals + eof
  val nonTerminals = lang.nonTerminals + startSymbol
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
  def goto(state: State): Map[(State, Symbol), State] =
    state.filterNot(_.after.isEmpty)
      .groupBy(_.after.head)
      .map((symbol, gotoState) => ((state, symbol), closure(gotoState.map(_.shift))))

  lazy val lr0Automaton: Map[(State, Symbol), State] =
    def computeAutomaton(current: Map[(State, Symbol), State], computed: Set[State], frontier: Set[State]): Map[(State, Symbol), State] =
      val newEdges = frontier.flatMap(goto)
      val newStates = newEdges.map(_._2) -- computed
      if newStates.isEmpty then current ++ newEdges
      else
        computeAutomaton(current ++ newEdges, computed ++ frontier, newStates)
    val startState = closure(Set(augmentedRule.toItem))
    computeAutomaton(Map.empty, Set.empty, Set(closure(startState)))

  extension[K, V] (map: Map[K, Set[V]])
    def addToValues(key: K, value: V) = map + (key -> (map.getOrElse(key, Set.empty) + value))

  /** Least fixed point of current, over the graph given by edges */
  def lfp[T](edges: Set[(T, T)])(current: Set[(T, T)]): Set[(T,T)] =
    //compute the new pairs
    val next = for
      (el, er) <- edges
      (cl, cr) <- current
      if er == cl
      if !current.contains((el, cr))
    yield (el, cr)
    if next.isEmpty then current
    else lfp(edges)(current ++ next)

  type SymPair = (Symbol, Symbol)
  /** Given a set of pairs (a,b), it returns a map:
   *  {a -> s| s contains all elements b, such that (a,b)\in pairs} */
  def groupPairs(pairs: Set[SymPair]) =
    pairs.groupMapReduce(_._1)((p: SymPair) => Set(p._2))(_ ++ _)

  /** Pudu does not support null productions, so FIRST reduces to reachability
   *  in the graph given by the first symbol of each production. */
  lazy val first: Map[Symbol, Set[Symbol]] =
    val edges: Set[SymPair] = rules.map(rule => rule.left -> rule.right.head)
    def firstLFP = lfp(edges)
    /* The LFP starts from {(t,t)| t is a terminal} */
    val start = terminals.map(t => t -> t)
    groupPairs(firstLFP(start))

  /** FOLLOW is also computed as a LFP but with reversed edges: from the last
   *  symbol of the rhs to the left symbol. This FOLLOWs the definition,
   *  because for each production X ::= ...Z, FOLLOW(X)\subseteq FOLLOW(Z) */
  lazy val follow: Map[Symbol, Set[Symbol]] =
    val edges: Set[SymPair] = rules.map(rule => rule.right.last -> rule.left)
    def followLFP = lfp(edges)
    /* the starting set is
     * { (N, T): there exists a production U ::= ...Nz..., and T\in FIRST(z) } */
    val start = for
      rule <- rules
      if rule.right.size >= 2
      pair <- rule.right.sliding(2)
      if isNonTerminal(pair(0))
      elem <- first(pair(1))
    yield (pair(0), elem)
    groupPairs(followLFP(start))
