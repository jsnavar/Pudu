package pudu.parser.generator

import pudu.parser._
import pudu.grammar._

/** Generates a human readable report on parser */
class LRReport[Tree, Token <: reflect.Enum](parser: LRParserGenerator[Tree, Token]):

  private def indentNL[T](ntabs: Int, c: Iterable[T]): String =
    c.map(x => "\t" * ntabs + x).mkString("\n")

  def rules: String = indentNL(1, parser.rules)

  def states: String =
    parser.indexedStates.toSeq.sortBy(_._2)
      .map{ case (state, idx) =>
             s"\tState $idx:\n" + indentNL(2, state)}
      .mkString("\n\n")

  def first: String =
    parser.first.map {
      case (symbol, fs) =>
        val fsString = fs.mkString(", ")
        s"\tFIRST[$symbol] = \n\t\t{ ${fsString} }"
    }.mkString("\n")

  def follow: String =
    parser.follow.map {
      case (symbol, fs) =>
        val fsString = fs.mkString(", ")
        s"\tFOLLOW[$symbol] = \n\t\t{ ${fsString} }"
    }.mkString("\n")

  def lr0Automaton: String =
    parser.lr0Automaton.map {
      case ((from, symbol), to) =>
        val fromIdx = parser.indexedStates(from)
        val toIdx = parser.indexedStates(to)
        s"\t(State: $fromIdx, Symbol: $symbol) => State: $toIdx"
    }.mkString("\n")

  def actionTable: String =
    def actionString(key: (Int, Int), action: SRAction) =
      val (state, ordinal) = key
      val tokName = parser.tokenNames.getOrElse(ordinal, "UNK")
      s"(state($state), $tokName) --> $action"
    indentNL(1, parser.actionTable.map(actionString))

  def reportAll: String =
    /* transform a Map[(K1, K2), V] to a Map[K1, Set((K2, V))] */
    def groupTable[K1,K2,V] (table: Map[(K1,K2),V]) =
      table.groupMap(_._1._1)
                    (entry => (entry._1._2 -> entry._2))

    val actions = groupTable(parser.actionTable)
    val goto = groupTable(parser.gotoTable)

    /* strings for actions and goto entries */
    def actionString(ordinal: Int, action: SRAction) =
      val tokName = parser.tokenNames.getOrElse(ordinal, "UNK")
      s"on $tokName --> $action"
    def gotoString(nt: Symbol, toState: Int) =
      s"goto: ${nt} --> ${toState}"

    /* generate report */
    parser.indexedStates.toSeq.sortBy(_._2)
      .map { case (state, idx) =>
               Seq(s"\tState $idx:",
                   indentNL(2, state),
                   indentNL(2, actions(idx).map(actionString)),
                   indentNL(2, goto.getOrElse(idx, Set.empty).map(gotoString)))
                     .filterNot(_.isEmpty).mkString("\n") }
      .mkString("\n\n")
