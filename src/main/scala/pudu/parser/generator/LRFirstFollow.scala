package pudu.parser.generator

import pudu.grammar._

/** Computes FIRST and FOLLOW of the grammar from 'lang', after grammar augmentation */
class LRFirstFollow[Tree, Token <: reflect.Enum](grammar: Grammar[Tree, Token]):
  import grammar._

//  val startSymbol = NonTerminal[Tree]("S'")
//  val eof: Terminal[Token] = lang.eof

//  val augmentedRule: RuleT = Rule(startSymbol, Seq(lang.start), _.head.asInstanceOf[Tree])

//  val rules = lang.rules + augmentedRule

//  val terminals = lang.terminals + eof
//  val nonTerminals = lang.nonTerminals + startSymbol

//  def isTerminal(symbol: Symbol) = terminals.contains(symbol)
//  def isNonTerminal(symbol: Symbol) = nonTerminals.contains(symbol)


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
