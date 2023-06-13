package pudu.grammar

/** An specification for a language. It exposes:
 *  - rules: a set of [[pudu.grammar.Rule]],
 *  - nonTerminals: a set of [[pudu.grammar.Symbol]] corresponding to non terminals
 *  - terminals: a set of [[pudu.grammar.Symbol]] corresponding to terminals
 *  - start: the start symbol of the grammar
 *  - precedence: precedence and associativity rules
 *
 *  Types Tree and Token refer to the types used for NonTerminal and Terminal respectively */
abstract class LanguageSpec[Tree, Token <: scala.reflect.Enum]:

  // Rules are collected into a private mutable set, which is later "fixed" to the immutable set 'rules'
  private val rulesSet = scala.collection.mutable.HashSet[Rule[Tree,Token]]()
  lazy val rules = rulesSet.toSet

  // Non Terminals are the left hand sides of rules.
  lazy val nonTerminals = rules.map(_.left).toSet[Symbol]

  // Terminals are all Symbols in the right hand side of rules, minus the non terminals
  lazy val terminals =
    val possibleTerminals = rules.flatMap(_.right) -- nonTerminals
    val undefNonTerminals = possibleTerminals.filter(_.isInstanceOf[NonTerminal[_]])
    if !undefNonTerminals.isEmpty then
      throw UndefinedNonTerminalException(undefNonTerminals)
    possibleTerminals

  val start: Symbol
  val eof: Terminal[Token]
  val error: Terminal[Token]
  val precedence: Precedence = Precedence.empty

  type TupSymData[T <: NonEmptyTuple] = Tuple.Map[T, SymData]

  /** Rule definition methods. Given a NonTerminal left, a rule can be defined as
    {{{    (left ::= (r1, r2, r3)) { (v1, v2 v3) => ... }      }}}
    * where the types of v1, v2, and v3 are inferred from 'r1', 'r2', and 'r3' */
  extension [R <: Tree] (left: NonTerminal[R])
    protected inline def ::= [Tup <: NonEmptyTuple] (inline right: Tup)(inline fn: TupSymData[Tup] => R)(using IsBoundedTuple[Tree, Token, Tup]): Unit =
      rulesSet += Rule(left,
                       right.toList.asInstanceOf[Seq[Symbol]], // this cast is safe by the IsBoundedTuple clause above
                       toSeqFnTuple(fn))
    /* This is defined to allow syntax (left ::= right) for unit productions */
    protected inline def ::= [T <: Symbol] (inline right: T)(inline fn: SymData[T] => R)(using IsBounded[Tree, Token, T]): Unit =
      rulesSet += Rule(left, Seq(right), toSeqFn(fn))

case class UndefinedNonTerminalException(nonTerminals: Set[Symbol]) extends Exception:
  override def getMessage() =
    if nonTerminals.size == 1 then s"Missing productions for non terminal ${nonTerminals.head}"
    else
      val ntStr = nonTerminals.mkString(", ")
      s"Non terminals { $ntStr } were used without productions"
