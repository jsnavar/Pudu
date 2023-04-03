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
   * Terminal and NonTerminal, we define the union type Sym to mean either a NonTerminal[Tree]
   * or a Terminal[Token] */
  type Par = Tree | Token
  type Sym[T <: Par] = NonTerminal[T] | Terminal[T]

  // Rules are collected into a private mutable set, which is later "fixed" to the immutable set 'rules'
  private var rulesSet = scala.collection.mutable.HashSet[Rule]()
  lazy val rules = rulesSet.toSet

  // Non Terminals are the left hand of rules.
  lazy val nonTerminals = rules.map(_.left)
  // Terminals are all Symbols in the right hand side of rules, minus the non terminals
  lazy val terminals = rules.flatMap(_.right).diff(nonTerminals)

  val start: Symbol
  val precedence: Precedence = Precedence.empty

  /** Rule definition methods. Given a NonTerminal left, a rule can be defined as
    {{{    (left ::= (r1, r2, r3)) { (v1, v2 v3) => ... }      }}}
    * where the types of v1, v2, and v3 are inferred from 'r1', 'r2', and 'r3' */
  /* (Can this be generated using macros or quote reflection?) */
  extension [R <: Tree] (left: NonTerminal[R])
    protected def ::= [T1 <: Par] (right: Sym[T1])(fn: T1 => R): Unit =
      rulesSet += Rule(left, Seq(right), stackFunction(1, fn))

    protected def ::= [T1 <: Par, T2 <: Par]
        (right:(Sym[T1], Sym[T2]))(fn: ((T1,T2)) => R): Unit =
      rulesSet += Rule(left, Seq(right(0), right(1)), stackFunction(2, untupled(fn).curried))

    protected def ::= [T1 <: Par, T2 <: Par, T3 <: Par]
        (right: (Sym[T1],Sym[T2],Sym[T3]))
        (fn: ((T1,T2,T3)) => R): Unit =
      rulesSet += Rule(left, Seq(right(0), right(1), right(2)), stackFunction(3, untupled(fn).curried))

    protected def ::= [T1 <: Par, T2 <: Par, T3 <: Par, T4 <: Par]
        (right: (Sym[T1],Sym[T2],Sym[T3],Sym[T4]))
        (fn: ((T1,T2,T3,T4)) => R): Unit =
      rulesSet += Rule(left, Seq(right(0), right(1), right(2), right(3)), stackFunction(4, untupled(fn).curried))

    protected def ::= [T1 <: Par, T2 <: Par, T3 <: Par, T4 <: Par, T5 <: Par]
        (right: (Sym[T1], Sym[T2], Sym[T3], Sym[T4], Sym[T5]))
        (fn: ((T1,T2,T3,T4,T5)) => R): Unit =
      rulesSet += Rule(left, Seq(right(0), right(1), right(2), right(3), right(4)), stackFunction(5, untupled(fn).curried))

    protected def ::= [T1 <: Par, T2 <: Par, T3 <: Par, T4 <: Par, T5 <: Par, T6 <: Par]
        (right: (Sym[T1], Sym[T2], Sym[T3], Sym[T4], Sym[T5], Sym[T6]))
        (fn: ((T1,T2,T3,T4,T5,T6)) => R): Unit =
      rulesSet += Rule(left, Seq(right(0), right(1), right(2), right(3), right(4), right(5)), stackFunction(6, untupled(fn).curried))

    protected def ::= [T1 <: Par, T2 <: Par, T3 <: Par, T4 <: Par, T5 <: Par, T6 <: Par, T7 <: Par]
        (right: (Sym[T1], Sym[T2], Sym[T3], Sym[T4], Sym[T5], Sym[T6], Sym[T7]))
        (fn: ((T1,T2,T3,T4,T5,T6,T7)) => R): Unit =
      rulesSet += Rule(left, Seq(right(0), right(1), right(2), right(3), right(4), right(5), right(6)), stackFunction(7, untupled(fn).curried))

    protected def ::= [T1 <: Par, T2 <: Par, T3 <: Par, T4 <: Par, T5 <: Par, T6 <: Par, T7 <: Par, T8 <: Par]
        (right: (Sym[T1], Sym[T2], Sym[T3], Sym[T4], Sym[T5], Sym[T6], Sym[T7], Sym[T8]))
        (fn: ((T1,T2,T3,T4,T5,T6,T7,T8)) => R): Unit =
      rulesSet += Rule(left, Seq(right(0), right(1), right(2), right(3), right(4), right(5), right(6), right(7)), stackFunction(8, untupled(fn).curried))

