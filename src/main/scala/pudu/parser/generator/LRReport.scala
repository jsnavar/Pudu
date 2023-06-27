package pudu.parser.generator

import pudu.parser._
import pudu.grammar._

/** Generates a human readable report on a parser generator */
class LRReport[Tree, Token <: reflect.Enum](
  _rules: Set[Rule[Tree, Token]],
  _tokenNames: Map[Int, String],
  _indexedStates: Map[State[Tree, Token], Int],
  _lrAutomaton: Map[(State[Tree, Token], Symbol), State[Tree, Token]],
  _actionTable: Map[(Int,Int), Set[SRAction]],
  _gotoTable: Map[(Int,Symbol), Int]):

  private def indentNL[T](ntabs: Int, c: Iterable[T]): String =
    c.map(x => "\t" * ntabs + x).mkString("\n")

  private def iterToStr[T](entries: Iterable[T]): String =
    if entries.size == 1 then entries.head.toString
    else s"[${entries.mkString(", ")}]"

  lazy val rules: String = indentNL(1, _rules)

  lazy val states: String =
    _indexedStates.toSeq.sortBy(_._2)
      .map{ case (state, idx) =>
             s"\tState $idx:\n" + indentNL(2, state)}
      .mkString("\n\n")

  lazy val lrAutomaton: String =
    _lrAutomaton.map {
      case ((from, symbol), to) =>
        val fromIdx = _indexedStates(from)
        val toIdx = _indexedStates(to)
        s"\t(State: $fromIdx, Symbol: $symbol) => State: $toIdx"
    }.mkString("\n")

  lazy val actionTable: String =
    def actionString(key: (Int, Int), actions: Set[SRAction]) =
      val (state, ordinal) = key
      val tokName = _tokenNames.getOrElse(ordinal, "UNK")
      val actionsStr = iterToStr(actions)
      s"(state($state), $tokName) --> $actionsStr"
    indentNL(1, _actionTable.map(actionString))

  lazy val all: String =
    /* transform a Map[(K1, K2), V] to a Map[K1, Set((K2, V))] */
    def groupTable[K1,K2,V] (table: Map[(K1,K2),V]) =
      table.groupMap(_._1._1)
                    (entry => (entry._1._2 -> entry._2))

    val actions = groupTable(_actionTable)
    val goto = groupTable(_gotoTable)

    /* strings for actions and goto entries */
    def actionString(ordinal: Int, actions: Set[SRAction]) =
      val tokName = _tokenNames.getOrElse(ordinal, "UNK")
      val actionsStr = iterToStr(actions)
      s"on $tokName --> $actionsStr"
    def gotoString(nt: Symbol, toState: Int) =
      s"if ${nt} goto ${toState}"

    /* generate report */
    _indexedStates.toSeq.sortBy(_._2)
      .map { case (state, idx) => s"\tState $idx:\n" +
               Seq(indentNL(2, state),
                   indentNL(2, actions.getOrElse(idx, Set.empty).map(actionString)),
                   indentNL(2, goto.getOrElse(idx, Set.empty).map(gotoString)))
                     .filterNot(_.isEmpty).mkString("\n\t\t---\n") }
      .mkString("\n\n")

  def writeAllToFile(path: String): Unit =
    import scala.util.Try
    import java.io.{File, PrintWriter}

    Try {
      val file = File(path)
      val dir = File(file.getParent())
      dir.mkdirs()
      PrintWriter(file)
    } map { printer =>
      printer.print("Productions:\n")
      printer.print(rules)
      printer.print("\nStates:\n")
      printer.print(all)
      printer.close()
    }
