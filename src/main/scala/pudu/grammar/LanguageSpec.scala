package pudu.grammar

/** An specification for a language. It exposes:
 *  - rules: a set of [[pudu.grammar.Rule]],
 *  - nonTerminals: a set of [[pudu.grammar.Symbol]] corresponding to non terminals
 *  - terminals: a set of [[pudu.grammar.Symbol]] corresponding to terminals
 *  - start: the start symbol of the grammar
 *  - precedence: precedence and associativity rules
 *
 *  Types Tree and Token refer to the types used for NonTerminal and Terminal respectively */
abstract class LanguageSpec[Tree, Token <: scala.reflect.Enum](using scala.util.NotGiven[reflect.Enum <:< Token]):

  // Rules are collected into a private mutable set, which is later "fixed" to the immutable set 'rules'
  private val rulesSet = scala.collection.mutable.HashSet[Rule[Tree,Token]]()
  lazy val rules = rulesSet.toSet

  // Non Terminals are the left hand sides of rules.
  lazy val nonTerminals = rules.map(_.left).toSet[Symbol]

  // Terminals are all Symbols in the right hand side of rules, minus the non terminals
  lazy val terminals =
    val possiblyTerminals = rules.flatMap(_.right) -- nonTerminals
    val undefNonTerminals = possiblyTerminals.filter(_.isInstanceOf[NonTerminal[_]])
    if !undefNonTerminals.isEmpty then
      throw UndefinedNonTerminalException(undefNonTerminals)
    possiblyTerminals

  val start: Symbol
  val eof: Terminal[Token]
  val error: Terminal[Token]
  val precedence: Precedence = Precedence.empty

  /** Rule definitions. Given a NonTerminal left, a rule can be defined as
    {{{    (left ::= (r1, r2, r3)) { (v1, v2 v3) => ... }      }}}
    * where the types of v1, v2, and v3 are inferred from 'r1', 'r2', and 'r3' */
  extension [T <: Tree] (left: NonTerminal[T])
    protected inline def ::= [Right] (inline right: Right)(inline fn: SymData[Right] => T)(using IsBounded[Tree, Token, Right]): Unit =
      val rightSeq = inline right match
        case s: Symbol => Seq(s)
        case t: NonEmptyTuple => t.toList.asInstanceOf[Seq[Symbol]] // this cast is safe by the IsBounded clause above

      rulesSet += Rule(left, rightSeq, toSeqFn(fn))

case class UndefinedNonTerminalException(nonTerminals: Set[Symbol]) extends Exception:
  override def getMessage() =
    if nonTerminals.size == 1 then s"Missing productions for non terminal ${nonTerminals.head}"
    else
      val ntStr = nonTerminals.mkString(", ")
      s"Non terminals { $ntStr } were used without productions"
