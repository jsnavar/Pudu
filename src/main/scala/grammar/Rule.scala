package pudu.grammar

/** Grammar rules/productions */


/** Rule of the form left ::= right, where left is a non terminal,
 *  and right a sequence of symbols.
 *  It is Serializable in order to allow writing the parser specification
 *  to a file */
abstract class Rule extends Serializable {
  val left: Symbol
  val right: Seq[Symbol]

  /** Function to 'reduce' (in a shift/reduce sense) by this rule.
   *  It takes the top right.size elements from the stack, computes the
   *  corresponding value, and returns a new stack with that value on top */
  def reduce(stack: Seq[Any]): Seq[Any]
}

/** Concrete implementations of Rules follow the standard library approach for
 *  Tuples, Products, and Functions. 
 *  It would be nice to generate this code using metaprogramming, but for now
 *  they are implemented explicitly */

case class Rule1[T1,R](left: NonTerminal[R], right: Seq[Symbol], fn: T1 => R) extends Rule:
  def reduce(stack: Seq[Any]) = fn(stack.head.asInstanceOf[T1]) +: stack.tail


/** fn is tupled in order to support syntax { t => ... t._1 ... } and { (arg1, arg2) => ... } */
case class Rule2[T1,T2,R](left: NonTerminal[R], right: Seq[Symbol], fn:((T1,T2)) => R) extends Rule:
  def reduce(stack: Seq[Any]) =
    val (top, bottom) = stack.splitAt(2)
    val args = (top(0), top(1)).asInstanceOf[(T1,T2)]
    fn(args) +: bottom

case class Rule3[T1,T2,T3,R](left: NonTerminal[R], right: Seq[Symbol], fn:((T1,T2,T3)) => R) extends Rule:
  def reduce(stack: Seq[Any]) =
    val (top, bottom) = stack.splitAt(3)
    val args = (top(0), top(1), top(2)).asInstanceOf[(T1,T2,T3)]
    fn(args) +: bottom

case class Rule4[T1,T2,T3,T4,R](left: NonTerminal[R], right: Seq[Symbol], fn:((T1,T2,T3,T4)) => R) extends Rule:
  def reduce(stack: Seq[Any]) =
    val (top, bottom) = stack.splitAt(4)
    val args = (top(0), top(1), top(2), top(3)).asInstanceOf[(T1,T2,T3,T4)]
    fn(args) +: bottom

case class Rule5[T1,T2,T3,T4,T5,R](left: NonTerminal[R], right: Seq[Symbol], fn:((T1,T2,T3,T4,T5)) => R) extends Rule:
  def reduce(stack: Seq[Any]) =
    val (top, bottom) = stack.splitAt(5)
    val args = (top(0), top(1), top(2), top(3), top(4)).asInstanceOf[(T1,T2,T3,T4,T5)]
    fn(args) +: bottom

case class Rule6[T1,T2,T3,T4,T5,T6,R](left: NonTerminal[R], right: Seq[Symbol], fn:((T1,T2,T3,T4,T5,T6)) => R) extends Rule:
  def reduce(stack: Seq[Any]) =
    val (top, bottom) = stack.splitAt(6)
    val args = (top(0), top(1), top(2), top(3), top(4), top(5)).asInstanceOf[(T1,T2,T3,T4,T5,T6)]
    fn(args) +: bottom

case class Rule7[T1,T2,T3,T4,T5,T6,T7,R](left: NonTerminal[R], right: Seq[Symbol], fn:((T1,T2,T3,T4,T5,T6,T7)) => R) extends Rule:
  def reduce(stack: Seq[Any]) =
    val (top, bottom) = stack.splitAt(7)
    val args = (top(0), top(1), top(2), top(3), top(4), top(5), top(6)).asInstanceOf[(T1,T2,T3,T4,T5,T6,T7)]
    fn(args) +: bottom

case class Rule8[T1,T2,T3,T4,T5,T6,T7,T8,R](left: NonTerminal[R], right: Seq[Symbol], fn:((T1,T2,T3,T4,T5,T6,T7,T8)) => R) extends Rule:
  def reduce(stack: Seq[Any]) =
    val (top, bottom) = stack.splitAt(8)
    val args = (top(0), top(1), top(2), top(3), top(4), top(5), top(6), top(7)).asInstanceOf[(T1,T2,T3,T4,T5,T6,T7,T8)]
    fn(args) +: bottom
