import pudu.grammar._

class FunctionsTest extends munit.FunSuite {
  test("seq length 1") {
    val f = (x: Int) => x * 3
    val sf: Seq[Int]=>Int = seq(1, f)

    val args = Seq(2,3,4)

    assertEquals(sf(args), 6)
  }

  test("seq length 4") {

    def f(x1: Int, x2: String, x3: Int, x4: String) =
      x2 + x1 + x3 + x4
    val sf: Seq[Int|String] => Int|String = seq(4, f.curried)
    val args : Seq[Int|String] = Seq(2, "str: ", 3, ".")
    assertEquals(sf(args), "str: 23.")
  }
}
