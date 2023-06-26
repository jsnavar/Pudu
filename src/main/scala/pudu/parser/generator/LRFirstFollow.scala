package pudu.parser.generator

import pudu.grammar._

/** Computes FIRST and FOLLOW of the grammar from 'lang', after grammar augmentation */
class LRFirstFollow[Tree, Token <: reflect.Enum](grammar: Grammar[Tree, Token]):
  import grammar.{rules, isNonTerminal, terminals, startSymbol, eof}

  /** Given a set of pairs 'start: Set[(L, R)]' and a graph 'edges: Set[(L, L)]',
   *  this function computes:
   *  { (x: L, y: R) | there exists z: L, such that z is reachable from x on 'edges', and (z,y)\in 'start' } */
  def lfp[L, R](edges: Set[(L, L)], start: Set[(L, R)]): Set[(L,R)] =
    val invMap = edges.groupMap(_._2)(_._1)
    def impl(acc: Set[(L, R)], step: Set[(L, R)]): Set[(L, R)] =
      val next = for
        (stepLeft, stepRight) <- step
        prev <- invMap.getOrElse(stepLeft, Set.empty)
        newEdge = (prev, stepRight)
        if !acc.contains(newEdge)
      yield newEdge
      if next.isEmpty then acc
      else impl(acc ++ next, next)
    impl(start, start)

  /** Given a set of pairs (a,b), groupPairs returns a map:
   *  {a -> s| s contains all elements b, such that (a,b)\in pairs} */
  def groupPairs[L, R](pairs: Set[(L,R)]): Map[L, Set[R]] =
    pairs.groupMap(_._1)(_._2)

  type SymPair = (Symbol, Symbol)

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
