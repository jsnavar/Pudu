package pudu.parser.generator

import pudu.parser._
import pudu.grammar._

/** Generates a human readable report on parser */
class SLRReport[Tree, Token <: reflect.Enum](parser: SLRParserGenerator[Tree, Token]):

  def indentNL[T](ntabs: Int, c: Iterable[T]): String =
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
    parser.first.map {
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

  def actions: String = ???

