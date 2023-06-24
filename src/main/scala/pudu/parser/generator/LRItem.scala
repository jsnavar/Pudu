package pudu.parser.generator

import pudu.grammar.{Rule, Symbol, Terminal}

/** Items are rules with a dot somewhere in the right side. This is represented with two Seq, for before
 *  and after the dot */
case class LRItem[Tree,Token <: reflect.Enum](left: Symbol, before: Seq[Symbol], after: Seq[Symbol], context: Option[Terminal[Token]], rule: Rule[Tree,Token]):
  /** shifts the dot one place to the right. Caller must ensure that 'after' is not empty before calling */
  def shift = copy(before = before :+ after.head, after = after.tail)
  override def toString =
    def sp[T](seq: Seq[T]) = seq.mkString(" ")
    val ctxStr = context.map(term => s" | ${term.toString}").getOrElse("")
    s"$left ::= ${sp(before)} · ${sp(after)}${ctxStr}"

object LRItem:
  def from[Tree, Token <: reflect.Enum](rule: Rule[Tree, Token], ctx: Option[Terminal[Token]]) =
    LRItem(rule.left, Seq.empty[Symbol], rule.right, ctx, rule)

extension[Tree, Token <: reflect.Enum] (rule: Rule[Tree, Token])
  def toItem = LRItem.from(rule, None)
  def toItem(ctx: Terminal[Token]) = LRItem.from(rule, Some(ctx))

type State[Tree, Token <: reflect.Enum] = Set[LRItem[Tree, Token]]
