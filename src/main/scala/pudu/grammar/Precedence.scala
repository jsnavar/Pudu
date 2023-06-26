package pudu.grammar

enum Assoc:
  case Left
  case Right
  case NonAssoc

enum Side:
  case Left
  case Right
  case Neither
  case Error

/** Operator precedence class.
 *  nth entry in levels represents the associativity of the nth precedence level,
 *  and symPrec maps symbols to their levels */
class Precedence(levels: Seq[Assoc], val symPrec: Map[Symbol, Int]):
  /** adds a new precedence level with symbols 'symbols', and associativity 'assoc' */
  def appended(symbols: Iterable[Symbol], assoc: Assoc) =
    val lv = levels.size
    require(symbols.forall(!symPrec.contains(_)), "Precedence of a symbol must be declared at most once")
    new Precedence(levels :+ assoc, symPrec ++ symbols.map(_ -> lv))

  /** helper methods */
  def left(symbols: Symbol*) = appended(symbols, Assoc.Left)
  def right(symbols: Symbol*) = appended(symbols, Assoc.Right)
  def nonassoc(symbols: Symbol*) = appended(symbols, Assoc.NonAssoc)

  /** returns the side with higher precedence, Neither if they are not comparable, and Error
   *  if they are nonassoc */
  def max(left: Symbol, right: Symbol): Side =
    if !symPrec.contains(left) || !symPrec.contains(right) then Side.Neither
    else
      val leftPrec = symPrec(left)
      val rightPrec = symPrec(right)
      if leftPrec == rightPrec then levels(leftPrec) match
        case Assoc.Left => Side.Left
        case Assoc.Right => Side.Right
        case Assoc.NonAssoc => Side.Error
      else if leftPrec > rightPrec then Side.Left else Side.Right

object Precedence:
  def empty = new Precedence(Seq.empty[Assoc], Map.empty[Symbol, Int])
  def apply() = empty
