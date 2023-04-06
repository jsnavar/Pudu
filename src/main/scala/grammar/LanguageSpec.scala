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
  /* As [[pudu.grammar.Symbol]] is not parametrized, and could have subclasses different from
   * Terminal and NonTerminal, we define the union type Sym to mean the union of NonTerminal[Tree]
   * and Terminal[Token]. This usually is very big (Any?), but it adds some safety (sometimes) */
  type Par = Tree | Token
  type Sym[T <: Par] = NonTerminal[T] | Terminal[T]

  // Rules are collected into a private mutable set, which is later "fixed" to the immutable set 'rules'
  private var rulesSet = scala.collection.mutable.HashSet[Rule[Tree,Par]]()
  lazy val rules = rulesSet.toSet

  // Non Terminals are the left hand of rules.
  lazy val nonTerminals = rules.map(_.left).toSet[Symbol]
  // Terminals are all Symbols in the right hand side of rules, minus the non terminals
  lazy val terminals = rules.flatMap(_.right).diff(nonTerminals)

  val start: Symbol
  val precedence: Precedence = Precedence.empty

  /** Rule definition methods. Given a NonTerminal left, a rule can be defined as
    {{{    (left ::= (r1, r2, r3)) { (v1, v2 v3) => ... }      }}}
    * where the types of v1, v2, and v3 are inferred from 'r1', 'r2', and 'r3' */
  extension [R <: Tree] (left: NonTerminal[R])
    protected inline def ::= [T1 <: Par] (inline right: Sym[T1])(inline fn: T1 => R): Unit =
      rulesSet += Rule[Tree, Par](left, Seq(right), seq(1, fn))

    protected inline def ::= [T1 <: Par, T2 <: Par]
        (inline right:(Sym[T1], Sym[T2]))(inline fn: ((T1,T2)) => R): Unit =
      rulesSet += Rule[Tree, Par](left, Seq(right(0), right(1)), seq(2, untupled(fn).curried))

    protected inline def ::= [T1 <: Par, T2 <: Par, T3 <: Par]
        (inline right: (Sym[T1],Sym[T2],Sym[T3]))
        (inline fn: ((T1,T2,T3)) => R): Unit =
      rulesSet += Rule[Tree, Par](left, Seq(right(0), right(1), right(2)), seq(3, untupled(fn).curried))

    protected inline def ::= [T1 <: Par, T2 <: Par, T3 <: Par, T4 <: Par]
        (inline right: (Sym[T1],Sym[T2],Sym[T3],Sym[T4]))
        (inline fn: ((T1,T2,T3,T4)) => R): Unit =
      rulesSet += Rule[Tree, Par](left, Seq(right(0), right(1), right(2), right(3)), seq(4, untupled(fn).curried))

    protected inline def ::= [T1 <: Par, T2 <: Par, T3 <: Par, T4 <: Par, T5 <: Par]
        (inline right: (Sym[T1], Sym[T2], Sym[T3], Sym[T4], Sym[T5]))
        (inline fn: ((T1,T2,T3,T4,T5)) => R): Unit =
      rulesSet += Rule[Tree, Par](left, Seq(right(0), right(1), right(2), right(3), right(4)), seq(5, untupled(fn).curried))

    protected inline def ::= [T1 <: Par, T2 <: Par, T3 <: Par, T4 <: Par, T5 <: Par, T6 <: Par]
        (inline right: (Sym[T1], Sym[T2], Sym[T3], Sym[T4], Sym[T5], Sym[T6]))
        (inline fn: ((T1,T2,T3,T4,T5,T6)) => R): Unit =
      rulesSet += Rule[Tree, Par](left, Seq(right(0), right(1), right(2), right(3), right(4), right(5)), seq(6, untupled(fn).curried))

    protected inline def ::= [T1 <: Par, T2 <: Par, T3 <: Par, T4 <: Par, T5 <: Par, T6 <: Par, T7 <: Par]
        (inline right: (Sym[T1], Sym[T2], Sym[T3], Sym[T4], Sym[T5], Sym[T6], Sym[T7]))
        (inline fn: ((T1,T2,T3,T4,T5,T6,T7)) => R): Unit =
      rulesSet += Rule[Tree, Par](left, Seq(right(0), right(1), right(2), right(3), right(4), right(5), right(6)), seq(7, untupled(fn).curried))

    protected inline def ::= [T1 <: Par, T2 <: Par, T3 <: Par, T4 <: Par, T5 <: Par, T6 <: Par, T7 <: Par, T8 <: Par]
        (inline right: (Sym[T1], Sym[T2], Sym[T3], Sym[T4], Sym[T5], Sym[T6], Sym[T7], Sym[T8]))
        (inline fn: ((T1,T2,T3,T4,T5,T6,T7,T8)) => R): Unit =
      rulesSet += Rule[Tree, Par](left, Seq(right(0), right(1), right(2), right(3), right(4), right(5), right(6), right(7)), seq(8, untupled(fn).curried))
