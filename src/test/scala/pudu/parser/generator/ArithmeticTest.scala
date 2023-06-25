import pudu.grammar.Rule
import pudu.parser.generator._

object ArithmeticTest:
  val ag = SimpleArithmetic.grammar.augmented
  val rules = ag.rules

  type Tree = Int | List[Int]
  def select(f: Rule[Tree, Token] => Boolean): State[Tree, Token] =
    rules.filter(f).map(_.toItem)
