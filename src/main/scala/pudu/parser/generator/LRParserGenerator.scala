package pudu.parser.generator

import pudu.grammar._
import pudu.parser._

/** Generates an LR parser from the automaton, and reduce actions. */
class LRParserGenerator[Tree, Token <: scala.reflect.Enum](grammar: Grammar[Tree,Token],
                                                           lrAutomaton: Map[(State[Tree, Token], Symbol), State[Tree, Token]],
                                                           startState: State[Tree, Token],
                                                           indexedStates: Map[State[Tree, Token], Int],
                                                           reduceActions: Map[(State[Tree, Token], Terminal[Token]), Iterable[Rule[Tree, Token]]]):
  import grammar.{eof, isTerminal, precedence, error, terminalNames}

  type RuleT = Rule[Tree, Token]
  type StateT = State[Tree, Token]

  /* (StateIdx, TokenOrdinal) -> Set[Action] */
  type ActionTable = Map[(Int,Int), Set[SRAction]]

  /* ((StateIdx, TokenOrdinal), Set[Action]) */
  type ActionTableEntry = ((Int, Int), Set[SRAction])

  /* (StateIdx, NonTerminal) -> ToStateIdx */
  type GotoTable = Map[(Int,Symbol), Int]

  /* ((stateIndex, NonTerminal), stateIndex) */
  type GotoTableEntry = ((Int, Symbol), Int)

  type LRTables = (ActionTable, GotoTable)

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

  /** uses precedence to solve shift reduce conflicts. Shifts by default
   *  following tradition (https://www.gnu.org/software/bison/manual/html_node/How-Precedence.html) */
  def shiftReduceResolution(from: StateT, terminal: Terminal[Token], to: StateT, rule: RuleT): ActionTableEntry =
    /* precedence compares the last terminal in the reduced rule with the terminal to be shifted */
    rule.right.findLast(isTerminal) // findLast returns an Option
              .map(precedence.max(_, terminal)) match
      case None => shiftTo(from, terminal, to) // shift by default
      case Some(Side.Left) => reduceBy(from, terminal, rule)
      case Some(Side.Right) => shiftTo(from, terminal, to)
      case Some(Side.Neither) => shiftTo(from, terminal, to) // shift by default
      case Some(Side.Error) => shiftTo(from, terminal, to).merge(reduceBy(from, terminal, rule)) // combine shift and reduce

  def computeTables: LRTables =
    /* like reduceBy, but for an Iterable of rules */
    def reduceByAny(from: StateT, terminal: Terminal[Token], rules: Iterable[RuleT]): ActionTableEntry =
      val key = actionTableKey(from, terminal)
      key -> rules.map(SRAction.Reduce(_)).toSet[SRAction]

    /* Update LRTables for a given LR automaton edge */
    def updateTables(tables: LRTables, edge: ((StateT, Symbol), StateT)): LRTables =
      val ((fromState, symbol), toState) = edge
      val (actionsTable, gotoTable) = tables

      /* updates will depend on the type of symbol */
      symbol match
        case t: Terminal[_] =>
          val terminal = t.asInstanceOf[Terminal[Token]]

          // true by construction of the automaton
          assert(fromState.exists(item => !item.after.isEmpty && item.after.head == terminal))

          val shiftEntry = shiftTo(fromState, terminal, toState)
          val reduceByRules = reduceActions.getOrElse((fromState, terminal), Set.empty)

          /* Check if there is a SR conflict */
          val tableEntry = reduceByRules.size match
            case 0 => shiftEntry
            case 1 => shiftReduceResolution(fromState, terminal, toState, reduceByRules.head)
            /* sr resolution is not used if there is a rr conflict */
            case _ => reduceByAny(fromState, terminal, reduceByRules).merge(shiftEntry)

          (actionsTable + tableEntry, gotoTable)
        case _: NonTerminal[_] =>
          /* For non terminals, add an entry to the goto table */
          val fromStateIdx = indexedStates(fromState)
          val toStateIdx = indexedStates(toState)
          val gotoEntry = (fromStateIdx, symbol) -> toStateIdx
          (actionsTable, gotoTable + gotoEntry)

    // Transform the Map reduceAction into an initial ActionTable
    val reduce = reduceActions.map { case ((from, terminal), rules) => reduceByAny(from, terminal, rules) }

    // Compute tables. Actions table is partial because it lacks the accept condition */
    val (partialActionTable, gotoTable) = lrAutomaton.foldLeft((reduce, Map.empty))(updateTables)

    // add accept condition.

    // as the grammar is augmented, grammar.startRules contains
    // only one rule, which is of the form S' -> S. langStart refers to S
    val langStart = grammar.startRules.head.right.head
    val acceptState = lrAutomaton(startState, langStart)

    (partialActionTable + acceptOn(acceptState), gotoTable)

  lazy val (actionTable, gotoTable) = computeTables

  lazy val report = LRReport(grammar.rules,
                             grammar.terminalNames,
                             indexedStates,
                             lrAutomaton,
                             actionTable,
                             gotoTable)

  type ActionTableSingle = Map[(Int, Int), SRAction]

  /** LR parsing algorithm */
  def lrParse(action: ActionTableSingle, goto: GotoTable)(input: Iterator[Token]): Either[ErrorMsg, Tree] =
    def expectedTokens(state: Int) = action
        .keys.filter(_._1 == state) //tokens with a valid action table entry
        .map(_._2) //get ordinal values
        .map(terminalNames) //to names

    /* If input.hasNext is true, calls parsingImpl with the next token and stacks newStates and newStack,
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
          else Left(SyntaxError(token, terminalNames(token.ordinal), expectedTokens(state)))
    // start from the initial state with empty semantic stack. If the input is empty generates an EmptyInputError
    shiftToken(Seq(0), Seq.empty)(ErrorMsg.EmptyInputError)

  def parser: Iterator[Token] => Either[ErrorMsg, Tree] =
    val actions: ActionTableSingle =
      if actionTable.exists(_._2.size != 1) then
        val writeTo = "target/pudu/report"
        report.writeAllToFile(writeTo)

        throw UnresolvedConflictException(writeTo)
      else actionTable.transform((_,v) => v.head)
    lrParse(actions, gotoTable)
