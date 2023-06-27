import pudu.parser.generator.LRFirstFollow


class LRFirstFollowTest extends munit.FunSuite {
  import SimpleArithmetic._

  val lrff = LRFirstFollow(SimpleArithmetic.grammar.augmented)

  test("first") {
    val first = lrff.first
    assertEquals(first(expr), Set(intLit, funcId, lpar))
    assertEquals(first(exprList), Set(intLit, funcId, lpar))
    assertEquals(first(plus), Set(plus))
  }

  test("follow") {
    val follow = lrff.follow
    assertEquals(follow(expr), Set(rpar, plus, times, minus, comma, eof))
    assertEquals(Set(rpar), follow(exprList))
  }
}
