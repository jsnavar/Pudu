package pudu.parser.generator

import pudu.grammar._
import pudu.parser._

/** Items are rules with a dot somewhere in the right side. This is represented with two Seq, for before
 *  and after the dot */
case class Item[T,ST](left: NonTerminal[T], before: Seq[Symbol], after: Seq[Symbol], rule: Rule[T,ST]):
  /** shifts the dot one place to the right. Caller must ensure that 'after' is not empty before calling */
  def shift = Item(left, before :+ after.head, after.tail, rule)
  override def toString =
    def sp[T](seq: Seq[T]) = seq.mkString(" ")
    s"$left ::= ${sp(before)} · ${sp(after)}"

extension[T,ST] (rule: Rule[T,ST])
  def toItem: Item[T,ST] = Item(rule.left, Seq.empty[Symbol], rule.right, rule)

/** Base class for LR parser generators (SLR, LR(1), LALR) */
abstract class LRParserGenerator[Tree, Token <: scala.reflect.Enum](lang: LanguageSpec[Tree,Token]) extends ParserGenerator[Tree, Token]:
  type RuleT = Rule[Tree, Token]
  type ItemT = Item[Tree, Token]
  /** A state is just a set of Items. This may be changed in the future */
  type State = Set[ItemT]

  /* Grammar is augmented with a new start symbol */
  val startSymbol = NonTerminal[Tree]("S'")
  val eof: Terminal[Token] = lang.eof
  val error: Terminal[Token] = lang.error // For lexer errors
  val augmentedRule: RuleT = Rule(startSymbol, Seq(lang.start), _.head)

  val rules = lang.rules + augmentedRule
  val precedence = lang.precedence

  val terminals = lang.terminals + eof
  val nonTerminals = lang.nonTerminals + startSymbol
  val symbols = terminals ++ nonTerminals

  def isTerminal(symbol: Symbol) = terminals.contains(symbol)
  def isNonTerminal(symbol: Symbol) = nonTerminals.contains(symbol)

  /** maps a token ordinal to its name. This is only used to generate
   *  error messages, so it is declared as lazy */
  lazy val tokenNames = terminals.asInstanceOf[Set[Terminal[_]]]
    .map(t => t.ordinal -> t.name).toMap

  /** Closure of a state. Follows the standard definition */
  def closure(state: State): State =
    stateClosure(state, rules.map(_.toItem))

  def stateClosure(state: State, candidates: Set[ItemT]): State =
    // First, we take the symbols after the dot for each item in 'state'
    val starting = state.filterNot(_.after.isEmpty).map(_.after.head)
    // Then, select the candidates whose left part is in the 'starting' set
    val newItems = candidates.filter(item => starting.contains(item.left))
    // Continue recursively until no more items are added
    if newItems.isEmpty then state
    else stateClosure(state ++ newItems, candidates -- newItems)

  /** goto(state, symbol) follows the standard definition */
  def goto(state: State, symbol: Symbol): State =
    // Take the items from 'state', where 'symbol' appears immediately after the dot
    val nextKernel = state.filterNot(_.after.isEmpty).filter(_.after.head == symbol)
    // Shift those items
    val shifted = nextKernel.map(_.shift)
    // And compute the closure
    closure(shifted)

  /** computes the goto of every relevant symbol in 'state', returning a map such that
   *  goto(state)(symbol) equals goto(state, symbol) */
  def goto(state: State): Map[(State, Symbol), State] =
    state.filterNot(_.after.isEmpty)
      .groupBy(_.after.head)
      .map((symbol, gotoState) => ((state, symbol), closure(gotoState.map(_.shift))))

  /** Computes the LR0 automaton, i.e. a map that given a state and a symbol, returns
   *  the next state */
  lazy val lr0Automaton: Map[(State, Symbol), State] =
    def computeAutomaton(current: Map[(State, Symbol), State], computed: Set[State], frontier: Set[State]): Map[(State, Symbol), State] =
      // Compute goto for each state in frontier, getting a Map[(State, Symbol), State] with all the results
      val newEdges = frontier.flatMap(goto)
      // Finds all states reached in the previous step, that had not been visited before
      val newStates = newEdges.map(_._2) -- computed
      // Continue until no new state is found
      if newStates.isEmpty then current ++ newEdges
      else
        computeAutomaton(current ++ newEdges, computed ++ frontier, newStates)
    // Start from the augmented rule ( S' -> S )
    val startState = closure(Set(augmentedRule.toItem))
    computeAutomaton(Map.empty, Set.empty, Set(startState))

  /** Given map: Map[K, Set[V]], key: K and value: V, returns a new map with 'value'
   *  added to map(key) */
  extension[K, V] (map: Map[K, Set[V]])
    def addToValues(key: K, value: V) = map + (key -> (map.getOrElse(key, Set.empty) + value))

  /** Given a set of pairs 'current: Set[(L, R)]', and a graph 'edges: Set[(L, L)]',
   *  this function computes:
   *  { (x: L, y: R) | there exists z: L, such that z is reachable from x on 'edges', and (z,y)\in 'current' } */
  def lfp[L, R](edges: Set[(L, L)], current: Set[(L, R)]): Set[(L,R)] =
    //compute the new pairs
    val next = for
      (el, er) <- edges
      (cl, cr) <- current
      if er == cl
      if !current.contains((el, cr))
    yield (el, cr)
    if next.isEmpty then current
    else lfp(edges, current ++ next)

  type SymPair = (Symbol, Symbol)
  /** Given a set of pairs (a,b), groupPairs returns a map:
   *  {a -> s| s contains all elements b, such that (a,b)\in pairs} */
  def groupPairs[L, R](pairs: Set[(L,R)]): Map[L, Set[R]] =
    pairs.groupMapReduce(_._1)((p: (L,R)) => Set(p._2))(_ ++ _)

  /** Pudu does not support null productions, so FIRST reduces to reachability
   *  in the graph given by the first symbol of each production. */
  lazy val first: Map[Symbol, Set[Terminal[Token]]] =
    val edges: Set[SymPair] = rules.map(rule => rule.left -> rule.right.head)
    /* The LFP starts from {(t,t)| t is a terminal} */
    val start = terminals.map(t => t -> t.asInstanceOf[Terminal[Token]])
    groupPairs(lfp(edges, start))

  /** FOLLOW is also computed as reachability but with reversed edges: from the last
   *  symbol of the rhs to the left symbol. This follows the definition,
   *  because for each production X ::= ...Z, FOLLOW(X)\subseteq FOLLOW(Z) */
  lazy val follow: Map[Symbol, Set[Terminal[Token]]] =
    val edges: Set[SymPair] = rules.filter(rule => isNonTerminal(rule.right.last))
      .map(rule => rule.right.last -> rule.left)
    /* the starting set is
     * { (N, T): there exists a production U ::= ...Nz..., and T\in FIRST(z) } */
    val start = for
      rule <- rules
      if rule.right.size >= 2
      pair <- rule.right.sliding(2)
      if isNonTerminal(pair(0))
      elem <- first(pair(1))
    yield (pair(0), elem)
    val startPairs = start + (startSymbol -> eof) // S' -> $ is in follow by definition
    groupPairs(lfp(edges, startPairs))

  /** LR parsing algorithm */
  def lrParse(action: Map[(Int, Int), SRAction], goto: Map[(Int, Symbol), Int])(input: Iterator[Token]): Either[ErrorMsg, Tree] =
    def expectedTokens(state: Int) = action
        .keys.filter(_._1 == state) //tokens with a valid action table entry
        .map(_._2) //get ordinal values
        .map(tokenNames) //names

    /** Tries to get the next token. */
    def nextToken(last: Token, state: Int): Either[ErrorMsg, Token] =
      if !input.hasNext then
        val expected = expectedTokens(state)
        Left(InputEndedUnexpectedly(expected))
      else
        val tok = input.next
        if tok.ordinal == error.ordinal then
          // Lexical error
          Left(LexError(tok))
        else
          Right(tok)
    def syntaxError(token: Token, state: Int): ErrorMsg =
      val expected = expectedTokens(state)
      if token.ordinal == eof.ordinal then InputEndedUnexpectedly(expected)
      else if token.ordinal == error.ordinal then LexError(token)
      else SyntaxError(token, tokenNames(token.ordinal), expected)

    /** 'token' is the next token to be processed, 'states' the parsing states stack, and 'stack' the semantic
     *  actions stack */
    def parsingImpl(token: Token, states: Seq[Int], stack: Seq[Tree|Token]): Either[ErrorMsg, Tree] =
      // Check the SR action from the action table
      val state = states.head
      action.getOrElse((state, token.ordinal), Error) match
        case Shift(to) =>
          nextToken(token, state).flatMap {
            /* If nextToken succeeds, calls parsingImpl recursively with the next token and updated stacks.
             * If nextToken fails, then it just forwards the ErrorMsg */
            parsingImpl(_, to +: states, token +: stack)
          }
        case Reduce(ruleAny) =>
          val rule = ruleAny.asInstanceOf[RuleT]
          val updatedStates = states.drop(rule.arity)  // drops the top states from the 'states' stack
          val to = goto(updatedStates.head, rule.left) // get the next state from the goto table
          parsingImpl(token, to +: updatedStates, rule.reduce(stack)) // Recursive call, with the same token
        case Accept =>
          // the final value is the top of 'stack'
          Right(stack.head.asInstanceOf[Tree])
        case Error =>
          Left(syntaxError(token, states.head))
    if input.hasNext then
      parsingImpl(input.next, Seq(0), Seq.empty)
    else
      Left(EmptyInputError)
