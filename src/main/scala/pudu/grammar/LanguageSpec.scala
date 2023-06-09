package pudu.grammar

/** An class to specify grammars.
 *
 *  Types Tree and Token refer to the types used for NonTerminal and Terminal respectively. These are used
 *  as upper bounds for all symbols, which is needed for type safety of the parser.
 *  The clause NotGiven[Enum <:< Token] is used to forbid the definition of a language using Enum as Token:
 *  {{{ object SomeLanguage extends LanguageSpec[Any, reflect.Enum] }}} is not allowed. Doing so, would allow
 *  a parser to get an Iterator[T], for any enum T, which could result in a type error. */
abstract class LanguageSpec[Tree, Token <: scala.reflect.Enum](using scala.util.NotGiven[reflect.Enum <:< Token]):

  // Rules are collected into 'rules'.
  private val rules = scala.collection.mutable.HashSet[Rule[Tree,Token]]()

  // start symbol of the grammar
  val start: Symbol
  // terminal for the eof token. For example: val eof = Terminal[Token.EOF]
  val eof: Terminal[Token]
  // terminal for the (lexical) error token. val error = Terminal[Token.ERROR]
  val error: Terminal[Token]

  val precedence: Precedence = Precedence.empty

  /* The type of the data associated with a symbol or tuple of symbols.
   * We don't use {{{ case Tuple => Tuple.Map[T, SymData] }}} to avoid
   * matching nested tuples */
  type SymData[T] = T match
    case Terminal[t] => t
    case NonTerminal[t] => t
    case Tuple => Tuple.Map[T, [x] =>> x match
                                          case NonTerminal[u] => u
                                          case Terminal[u] => u]

  /* Checks if T is a NonTerminal[t <: Tree], a Terminal[t <: Token], or a tuple
   * where each element matches one of those types. */
  type IsBounded[T] = T match
    case NonTerminal[t] => t <:< Tree
    case Terminal[t] => t <:< Token
    case Tuple => SymData[T] <:< Tuple.Map[T, [x] =>> x match
                                           case NonTerminal[_] => Tree
                                           case Terminal[_] => Token ]

  /** Rule definition method.
   *  Given a NonTerminal left: NonTerminal[T], and symbols r1, r2, r3, ...,
   *  a production can be defined as:
    {{{    (left ::= (r1, r2, r3, ...)) { fn }      }}}
   *  where the type of the action 'fn' is inferred from
   *  'left', 'r1', 'r2', 'r3', ...
   *  The clause using IsBounded is used to enforce that all elements of
   *  right comply with NonTerminal[t <: Tree] or Terminal[+Token]. This could be
   *  simplified using that Terminal is covariant, but we favored a uniform
   *  definition for terminals and non terminals, considering that NonTerminal is
   *  invariant. */
  extension [T <: Tree] (left: NonTerminal[T])
    /* Here, Right can be a Symbol or a Tuple.
     * We include the Symbol case to allow defining unit productions without parenthesis, for example
     * {{{ (expr ::= literal) { ... } }}} instead of {{{ (expr ::= (literal)) { ... } }}} */
    protected inline def ::= [Right] (inline right: Right)(inline fn: SymData[Right] => T)(using IsBounded[Right]): Unit =
      /* In a [[pudu.grammar.Rule]], 'right' is a Seq[Symbol], and 'action' has type {{{ Seq[Tree | Token] => Tree }}},
       * so we need to convert both values. */
      val rightSeq = inline right match
        case s: Symbol => Seq(s)
        case t: NonEmptyTuple => t.toList.asInstanceOf[Seq[Symbol]] // this cast is safe by the IsBounded clause above

      rules += Rule(left, rightSeq, toSeqFn(fn))

  /* returns an equivalent [[pudu.grammar.Grammar]] */
  def grammar: Grammar[Tree, Token] =
    Grammar(rules.toSet, start, eof, error, precedence)
