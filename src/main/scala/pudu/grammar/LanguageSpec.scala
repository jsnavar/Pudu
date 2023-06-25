package pudu.grammar

/** An class to specify grammars using scala code.
 *
 *  Types Tree and Token refer to the types used for NonTerminal and Terminal respectively */
abstract class LanguageSpec[Tree, Token <: scala.reflect.Enum](using scala.util.NotGiven[reflect.Enum <:< Token]):

  // Rules are collected into a private mutable set
  private val rules = scala.collection.mutable.HashSet[Rule[Tree,Token]]()

  // start symbol of the grammar
  val start: Symbol
  // terminal for the eof token. For example: val eof = Terminal[Token.EOF]
  val eof: Terminal[Token]
  // terminal for the (lexical) error token. val error = Terminal[Token.ERROR]
  val error: Terminal[Token]
  // optional precedence object
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
  type IsBounded[Tree, Token, T] = T match
    case NonTerminal[t] => t <:< Tree
    case Terminal[t] => t <:< Token
    case Tuple => SymData[T] <:< Tuple.Map[T, [x] =>> x match
                                           case NonTerminal[_] => Tree
                                           case Terminal[_] => Token ]

  /** Rule definition method.
   *  Given a NonTerminal left: NonTerminal[T], and symbols r1, r2, r3, ...,
   *  a production can be defined as:
    {{{    (left ::= (r1, r2, r3, ...)) { fn }      }}}
   *  where the type of the semantic action 'fn' is inferred from
   *  'left', 'r1', 'r2', 'r3', ..., using the SymData type defined above.
   *  The clause using IsBounded is used to enforce that all elements of
   *  right comply with NonTerminal[t <: Tree] or Terminal[+Token]. This could be
   *  simplified using that Terminal is covariant, but we favored a uniform
   *  definition for terminals and non terminals, considering that NonTerminal is
   *  invariant. */
  extension [T <: Tree] (left: NonTerminal[T])
    /* Here, Right can be a Symbol or a Tuple.
     * We include the Symbol case to allow defining unit productions without parenthesis, for example
     * {{{ (expr ::= literal) { ... } }}} instead of {{{ (expr ::= (literal)) { ... } }}} */
    protected inline def ::= [Right] (inline right: Right)(inline fn: SymData[Right] => T)(using IsBounded[Tree, Token, Right]): Unit =
      val rightSeq = inline right match
        case s: Symbol => Seq(s)
        case t: NonEmptyTuple => t.toList.asInstanceOf[Seq[Symbol]] // this cast is safe by the IsBounded clause above

      rules += Rule(left, rightSeq, toSeqFn(fn))

  def grammar: Grammar[Tree, Token] =
    Grammar(rules.toSet, start, eof, error, precedence)
