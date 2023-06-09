package pudu.grammar

/** A Grammar. It contains:
 *  - rules: a set of [[pudu.grammar.Rule]],
 *  - nonTerminals: a set of [[pudu.grammar.Symbol]] corresponding to non terminals
 *  - terminals: a set of [[pudu.grammar.Symbol]] corresponding to terminals
 *  - start: the start symbol of the grammar
 *  - startRules: rules with 'start' as left symbol
 *  - eof: Terminal for EOF
 *  - error: Terminal for lexical errors
 *  - precedence: precedence and associativity rules */
case class Grammar[Tree, Token <: reflect.Enum](rules: Set[Rule[Tree, Token]],
                                                startSymbol: Symbol,
                                                eof: Terminal[Token],
                                                error: Terminal[Token],
                                                precedence: Precedence):
  val nonTerminals = rules.map(_.left).toSet[Symbol]

  val terminals =
    val possiblyTerminals = rules.flatMap(_.right) -- nonTerminals
    val undefNonTerminals = possiblyTerminals.filter(_.isInstanceOf[NonTerminal[_]])
    if !undefNonTerminals.isEmpty then
      throw UndefinedNonTerminalException(undefNonTerminals)
    possiblyTerminals + eof

  def isTerminal(symbol: Symbol) = terminals.contains(symbol)
  def isNonTerminal(symbol: Symbol) = nonTerminals.contains(symbol)

  /** maps a token ordinal to its name. */
  lazy val terminalNames =
    terminals.asInstanceOf[Set[Terminal[_]]]
      .map(t => t.ordinal -> t.name).toMap

  val startRules: Set[Rule[Tree, Token]] = rules.filter(_.left == startSymbol)

  /* Grammar augmented with a new start symbol */
  def augmented: Grammar[Tree, Token] =
      val newStart = NonTerminal[Tree](startSymbol.name + "'")
      val newRule = Rule[Tree, Token](newStart, Seq(startSymbol), _.head.asInstanceOf[Tree])
      copy(rules = rules + newRule, startSymbol = newStart)
