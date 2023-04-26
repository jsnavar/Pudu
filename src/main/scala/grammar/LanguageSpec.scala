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
  /* Semantic actions in shift-reduce parsers are computed using a stack, which holds either
   * tokens, or objects of type <: Tree. Here, we represent that with the union type StackTpe */
  type StackTpe = Tree | Token

  // Rules are collected into a private mutable set, which is later "fixed" to the immutable set 'rules'
  private val rulesSet = scala.collection.mutable.HashSet[Rule[Tree,Token]]()
  lazy val rules = rulesSet.toSet

  // Non Terminals are the left hand of rules.
  lazy val nonTerminals = rules.map(_.left).toSet[Symbol]
  // Terminals are all Symbols in the right hand side of rules, minus the non terminals
  lazy val terminals = rules.flatMap(_.right).diff(nonTerminals)

  val start: Symbol
  val eof: Terminal[Token]
  val error: Terminal[Token]
  val precedence: Precedence = Precedence.empty

  type TupSymData[T <: NonEmptyTuple] = Tuple.Map[T, SymData]

  /** Rule definition methods. Given a NonTerminal left, a rule can be defined as
    {{{    (left ::= (r1, r2, r3)) { (v1, v2 v3) => ... }      }}}
    * where the types of v1, v2, and v3 are inferred from 'r1', 'r2', and 'r3' */
  extension [R <: Tree] (left: NonTerminal[R])
    protected inline def ::= [Tup <: NonEmptyTuple] (inline right: Tup)(inline fn: TupSymData[Tup] => R)(using Tup <:< SymTuple[Tup]): Unit =
      rulesSet += Rule(left,
                       right.toList.asInstanceOf[Seq[Symbol]], // this cast is safe by the <:< clause above
                       toSeqFn(right.size, fn))
    /* This is defined to allow syntax (left ::= right) for unit productions, instead of (left ::= (right) */
    protected inline def ::= [T <: Symbol] (inline right: T)(inline fn: SymData[T] => R): Unit =
      rulesSet += Rule(left, Seq(right), toSeqFn(fn))
