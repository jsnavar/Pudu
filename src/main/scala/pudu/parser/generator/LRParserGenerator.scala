package pudu.parser.generator

import pudu.grammar._
import pudu.parser._

/** Base class for LR parser generators (SLR, LR(1), LALR) */
abstract class LRParserGenerator[Tree, Token <: scala.reflect.Enum](lang: LanguageSpec[Tree,Token]) extends ParserGenerator[Tree, Token]:

  type RuleT = Rule[Tree, Token]
  type ItemT = LRItem[Tree, Token]
  /** A state is just a set of Items. This may be changed in the future */
  type StateT = State[Tree, Token]

  /* (StateIdx, TokenOrdinal) -> Set[Action] */
  type ActionTable = Map[(Int,Int), Set[SRAction]]

  /* ((StateIdx, TokenOrdinal), Set[Action]) */
  type ActionTableEntry = ((Int, Int), Set[SRAction])

  /* (StateIdx, NonTerminal) -> ToStateIdx */
  type GotoTable = Map[(Int,Symbol), Int]
  /* ((stateIndex, nonTerminalSymbol), stateIndex) */
  type GotoTableEntry = ((Int, Symbol), Int)

  /* Grammar is augmented with a new start symbol */
  val startSymbol = NonTerminal[Tree]("S'")
  val eof: Terminal[Token] = lang.eof
  val error: Terminal[Token] = lang.error // For lexer errors
  val augmentedRule: RuleT = Rule(startSymbol, Seq(lang.start), _.head.asInstanceOf[Tree])

  val rules = lang.rules + augmentedRule
  val precedence = lang.precedence

  val terminals = lang.terminals + eof
  val nonTerminals = lang.nonTerminals + startSymbol

  def isTerminal(symbol: Symbol) = terminals.contains(symbol)
  def isNonTerminal(symbol: Symbol) = nonTerminals.contains(symbol)

  /* start state is the closure of the augmented rule */
  val startState: StateT = closure(Set(augmentedRule.toItem))

  /** maps a token ordinal to its name. This is only used to generate
   *  error messages, so it is declared as lazy */
  lazy val tokenNames = terminals.asInstanceOf[Set[Terminal[_]]]
    .map(t => t.ordinal -> t.name).toMap

  /*
   * Auxiliary functions
   */

  /** LR0 Closure of a state */
  def closure(state: StateT): StateT =
    stateClosure(state, rules.map(_.toItem))

  def stateClosure(state: StateT, candidates: Set[ItemT]): StateT =
    // First, we take the symbols after the dot for each item in 'state'
    val starting = state.filterNot(_.after.isEmpty).map(_.after.head)
    // Then, select the candidates whose left part is in the 'starting' set
    val newItems = candidates.filter(item => starting.contains(item.left))
    // Continue recursively until no more items are added
    if newItems.isEmpty then state
    else stateClosure(state ++ newItems, candidates -- newItems)

  /** goto(state, symbol) follows the standard definition */
  def goto(state: StateT, symbol: Symbol): StateT =
    // Take the items from 'state', where 'symbol' appears immediately after the dot
    val nextKernel = state.filterNot(_.after.isEmpty).filter(_.after.head == symbol)
    // Shift those items
    val shifted = nextKernel.map(_.shift)
    // And compute the closure
    closure(shifted)

  /** computes the goto of every relevant symbol in 'state', returning a map such that
   *  goto(state)(symbol) equals goto(state, symbol) */
  def goto(state: StateT): Map[(StateT, Symbol), StateT] =
    state.filterNot(_.after.isEmpty)
      .groupBy(_.after.head)
      .map((symbol, gotoState) => ((state, symbol), closure(gotoState.map(_.shift))))

  /** Computes the LR automaton, i.e. a map that given a state and a symbol, returns
   *  the next state */
  lazy val lrAutomaton: Map[(StateT, Symbol), StateT] =
    def computeAutomaton(current: Map[(StateT, Symbol), StateT], computed: Set[StateT], frontier: Set[StateT]): Map[(StateT, Symbol), StateT] =
      // Compute goto for each state in frontier, getting a Map[(StateT, Symbol), StateT] with all the results
      val newEdges = frontier.flatMap(goto)
      // Finds all states reached in the previous step, that had not been visited before
      val newStates = newEdges.map(_._2) -- computed
      // Continue until no new state is found
      if newStates.isEmpty then current ++ newEdges
      else
        computeAutomaton(current ++ newEdges, computed ++ frontier, newStates)
    computeAutomaton(Map.empty, Set.empty, Set(startState))

  lazy val indexedStates =
    val nonStartingStates = lrAutomaton.values.toSet
    val states = nonStartingStates + startState
    val numOfStates = states.size
    nonStartingStates.zip(1 until numOfStates).toMap + (startState -> 0)

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

  /*
   * Parsing tables
   */

  /** builds the action table key */
  def actionTableKey(from: StateT, terminal: Terminal[Token]) =
    val fromIdx = indexedStates(from)
    val terminalOrdinal = terminal.ordinal
    (fromIdx, terminalOrdinal)

  /** Action table entry for shift actions */
  def shiftTo(from: StateT, terminal: Terminal[Token], to: StateT): ActionTableEntry =
    val key = actionTableKey(from, terminal)
    val toIdx = indexedStates(to)
    key -> Set(SRAction.Shift(toIdx))

  /** Action table entry for reduce actions */
  def reduceBy(from: StateT, terminal: Terminal[Token], rule: RuleT): ActionTableEntry =
    val key = actionTableKey(from, terminal)
    key -> Set(SRAction.Reduce(rule))

  /** Action table entry for accept action */
  def acceptOn(acceptState: StateT): ActionTableEntry =
    val key = actionTableKey(acceptState, eof)
    key -> Set(SRAction.Accept)

  extension (entry: ActionTableEntry)
    def merge(that: ActionTableEntry): ActionTableEntry =
      require(entry._1 == that._1)
      entry._1 -> (entry._2 ++ that._2)

  /** uses precedence to solve shift reduce conflicts. Default is to shift, in
   *  accordance to tradition (https://www.gnu.org/software/bison/manual/html_node/How-Precedence.html) */
  def shiftReduceResolution(from: StateT, to: StateT)(rule: RuleT, terminal: Terminal[Token]): ActionTableEntry =
    val lastTerminalOfRule = rule.right.findLast(isTerminal)
    if !lastTerminalOfRule.isDefined then
      // shift by default
      shiftTo(from, terminal, to)
    else precedence.max(lastTerminalOfRule.get, terminal) match
      case Side.Left => reduceBy(from, terminal, rule)
      case Side.Right => shiftTo(from, terminal, to)
      case Side.Neither => shiftTo(from, terminal, to) // shift by default
      case Side.Error => shiftTo(from, terminal, to).merge(reduceBy(from, terminal, rule))

  lazy val reduceActions: Map[(StateT, Terminal[Token]), Set[RuleT]]

  lazy val (actionTable, gotoTable) =
    type LRTables = (ActionTable, GotoTable)

    def reduceTableEntry(from: StateT, terminal: Terminal[Token], rules: Set[RuleT]): ActionTableEntry =
      actionTableKey(from, terminal) -> rules.map(SRAction.Reduce(_)).toSet[SRAction]

    /* Update LRTables for a given LR automaton edge */
    def updateTables(tables: LRTables, edge: ((StateT, Symbol), StateT)): LRTables =
      val ((fromState, symbol), toState) = edge
      val (actionsTable, gotoTable) = tables

      /* updates will depend on the type of symbol */
      symbol match
        case t: Terminal[_] =>
          val terminal = t.asInstanceOf[Terminal[Token]]
          /* This assert is true by construction of the LR automaton */
          assert(fromState.exists(item => !item.after.isEmpty && item.after.head == terminal))

          /* table entry for the shift action */
          val shiftEntry = shiftTo(fromState, terminal, toState)

          /* Check if there is a SR conflict */
          val tableEntry =
            if reduceActions.contains(fromState, terminal) then
              /* If there is a conflict, apply resolution only if there is only one reduce action.
               * In case of a rr conflict, just return the full set of actions, including the shift */
              val rules = reduceActions(fromState, terminal)
              if rules.size == 1 then shiftReduceResolution(fromState, toState)(rules.head, terminal)
              else
                val reduce = reduceTableEntry(fromState, terminal, rules)
                reduce.merge(shiftEntry)
            else shiftEntry
          (actionsTable + tableEntry, gotoTable)
        case _: NonTerminal[_] =>
          /* For non terminals, the edge corresponds to an entry in the goto table */
          val fromStateIdx = indexedStates(fromState)
          val toStateIdx = indexedStates(toState)
          val gotoEntry = (fromStateIdx, symbol) -> toStateIdx
          (actionsTable, gotoTable + gotoEntry)

    // Transform the Map reduceAction into an initial ActionTable
    val reduce = reduceActions.map { case ((from, terminal), rules) => reduceTableEntry(from, terminal, rules) }
    // Compute tables. Actions table is partial because it lacks the accept condition */
    val (partialActionTable, gotoTable) = lrAutomaton.foldLeft((reduce, Map.empty))(updateTables)

    // add accept condition
    val acceptState = lrAutomaton(startState, lang.start)
    val acceptEntry = acceptOn(acceptState)

    (partialActionTable + acceptEntry, gotoTable)

  type ActionTableSingle = Map[(Int, Int), SRAction]

  /** LR parsing algorithm */
  def lrParse(action: ActionTableSingle, goto: GotoTable)(input: Iterator[Token]): Either[ErrorMsg, Tree] =
    def expectedTokens(state: Int) = action
        .keys.filter(_._1 == state) //tokens with a valid action table entry
        .map(_._2) //get ordinal values
        .map(tokenNames) //to names

    /* If input.hasNext, calls parsingImpl with the next token and stacks newStates and newStack,
     * otherwise returns Left(errorMsg) */
    def shiftToken(newStates: => Seq[Int], newStack: => Seq[Tree|Token])(errorMsg: => ErrorMsg) =
      input.nextOption() match
        case Some(nextToken) => parsingImpl(nextToken, newStates, newStack)
        case None => Left(errorMsg)

    /** 'token' is the next token to be processed, 'states' the parsing states stack, and 'stack' the semantic
     *  actions stack */
    def parsingImpl(token: Token, states: Seq[Int], stack: Seq[Tree|Token]): Either[ErrorMsg, Tree] =
      import ErrorMsg._
      import SRAction._

      val state = states.head
      action.getOrElse((state, token.ordinal), Error) match
        case Shift(to) =>
          // if input.hasNext, add 'to' to 'states', and push the current token to 'stack'.
          shiftToken(to +: states, token +: stack)(InputEndedUnexpectedly(expectedTokens(state)))
        case Reduce(ruleAny) =>
          val rule = ruleAny.asInstanceOf[RuleT]

          val clearedStates = states.drop(rule.arity)  // drops the top states from the 'states' stack
          val toState = goto(clearedStates.head, rule.left) // gets the next state from the goto table
          val updatedStates = toState +: clearedStates

          val updatedStack = rule.action(stack) +: stack.drop(rule.arity)

          parsingImpl(token, updatedStates, updatedStack) // Recursive call with the same token
        case Accept =>
          // the final value is the top of 'stack'
          Right(stack.head.asInstanceOf[Tree])
        case Error =>
          if token.ordinal == error.ordinal then Left(LexError(token))
          else if token.ordinal == eof.ordinal then Left(InputEndedUnexpectedly(expectedTokens(state)))
          else Left(SyntaxError(token, tokenNames(token.ordinal), expectedTokens(state)))
    // start from the initial state with empty semantic stack. If the input is empty generates an EmptyInputError
    shiftToken(Seq(0), Seq.empty)(ErrorMsg.EmptyInputError)

  def parser: Iterator[Token] => Either[ErrorMsg, Tree] =
    val actions: ActionTableSingle =
      if actionTable.exists(_._2.size != 1) then
        val writeTo = "target/pudu/report"
        val report = LRReport(this).writeAllToFile(writeTo)

        throw UnresolvedConflictException(writeTo)
      else actionTable.map((k,v) => k -> v.head)
    lrParse(actions, gotoTable)
