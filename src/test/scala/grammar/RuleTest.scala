import pudu.grammar._

// Tests for rule definitions

class RuleTest extends munit.FunSuite {
  enum Token:
    case LPar()
    case RPar()
    case Plus()
    case Times()
    case Minus()
    case Comma()
    case FuncId(id: String)
    case IntLit(value: Int)
    case EOF()

  val expr = NonTerminal[Int]
  val intLit = Terminal[Token.IntLit]
  val funcId = Terminal[Token.FuncId]
  val plus = Terminal[Token.Plus]
  val minus = Terminal[Token.Minus]
  val times = Terminal[Token.Times]
  val lpar = Terminal[Token.LPar]
  val rpar = Terminal[Token.RPar]
  val comma = Terminal[Token.Comma]
/****
  test("Rule1 definition") {
    val r1 = Rule1[Token.IntLit,Int](expr, Seq(intLit), _.value)
    val lit: Token.IntLit = Token.IntLit(5)
    assertEquals(r1.fn(lit), 5)
  }

  test("Rule1 reduction") {
    val r1: Rule = Rule1[Token.IntLit,Int](expr, Seq(intLit), _.value)
    val stack: Seq[Any] = Seq(Token.IntLit(1),2,3)
    val rstack: Seq[Any] = Seq(1,2,3)
    assertEquals(rstack, r1.reduce(stack))
  }

  test("Rule2 definition") {
    val r2 = Rule2[Token.Plus, Int, Int](expr, Seq(plus, expr), _._2)
    assertEquals(r2.fn(Token.Plus(),2), 2)
  }

  test("Rule2 reduction") {
    val r2: Rule = Rule2[Token.Plus, Int, Int](expr, Seq(plus, expr), _._2)
    val stack: Seq[Any] = Seq(Token.Plus(), 2, 3, 4)
    val rstack: Seq[Any] = Seq(2,3,4)
    assertEquals(rstack, r2.reduce(stack))
  }

  test("Rule3 definition") {
    val r3 = Rule3[Int, Token.Times, Int, Int](expr, Seq(expr, times, expr), 
                                               (l,_,r) => l * r)
    assertEquals(r3.fn(2, Token.Times(), 3), 6)
  }
  test("Rule3 reduction") {
    val r3 = Rule3[Int, Token.Minus, Int, Int](expr, Seq(expr, minus, expr),
                                               (l,_,r) => l - r)
    val stack: Seq[Any] = Seq(6, Token.Minus(), 2, 5)
    val rstack: Seq[Any] = Seq(4, 5)
    assertEquals(r3.reduce(stack), rstack)
  }

  test("Rule4 definition") {
    val r4 = Rule4[Token.FuncId, Token.LPar, Int, Token.RPar, Int](
      expr,
      Seq(funcId, lpar, expr, rpar), 
      (f,_,arg,_) => if f.id == "addOne" then arg + 1 else arg)
    assertEquals(r4.fn(Token.FuncId("addOne"), Token.LPar(), 5, Token.RPar()), 6)
  }
  test("Rule4 reduction") {
    val r4: Rule = Rule4[Token.FuncId, Token.LPar, Int, Token.RPar, Int](
      expr,
      Seq(funcId, lpar, expr, rpar), 
      (f,_,arg,_) => if f.id == "addOne" then arg + 1 else arg)
    val stack: Seq[Any] = Seq(Token.FuncId("identity"), Token.LPar(), 4, Token.RPar(), 8)
    val rstack: Seq[Any] = Seq(4,8)
    assertEquals(r4.reduce(stack), rstack)
  }

  test("Rule5 definition") {
    val r5 = Rule5[Token.LPar, Int, Token.Times, Int, Token.RPar, Int](
      expr,
      Seq(lpar, expr, times, expr, rpar),
      (_,l,_,r,_) => l * r)
    assertEquals(r5.fn(Token.LPar(), 2, Token.Times(), 3, Token.RPar()), 6)
  }
  test("Rule5 reduction") {
    val r5: Rule = Rule5[Token.LPar, Int, Token.Minus, Int, Token.RPar, Int](
      expr,
      Seq(lpar, expr, minus, expr, rpar),
      (_,l,_,r,_) => l - r)
    val stack: Seq[Any] = Seq(Token.LPar(), 2, Token.Minus(), 4, Token.RPar(), 2,3)
    val rstack: Seq[Any] = Seq(-2, 2, 3)
    assertEquals(r5.reduce(stack), rstack)
  }

  test("Rule6 definition") {
    val r6 = Rule6[Token.FuncId,Token.LPar,Int,Token.Comma,Int,Token.RPar, Int](
      expr,
      Seq(funcId, lpar, expr, comma, expr, rpar),
      (f,_,arg1,_,arg2,_) => if f.id == "max" then arg1.max(arg2) else arg1 * arg2)
    assertEquals(r6.fn(Token.FuncId("max"), Token.LPar(), 4, Token.Comma(), 5, Token.RPar()), 5)
  }
  test("Rule6 reduction") {
    val r6: Rule = Rule6[Token.FuncId,Token.LPar,Int,Token.Comma,Int,Token.RPar, Int](
      expr,
      Seq(funcId, lpar, expr, comma, expr, rpar),
      (f,_,arg1,_,arg2,_) => if f.id == "max" then arg1.max(arg2) else arg1 * arg2)
    val stack: Seq[Any] = Seq(Token.FuncId("mult"), Token.LPar(), 3, Token.Comma(), 9, Token.RPar(),Token.Minus(),2)
    val rstack: Seq[Any] = Seq(27, Token.Minus(), 2)
    assertEquals(r6.reduce(stack), rstack)
  }

  test("Rule7 definition") {
    val r7 = Rule7[Token.Minus,Token.FuncId,Token.LPar,Int,Token.Comma,Int,Token.RPar, Int](
      expr,
      Seq(minus, funcId, lpar, expr, comma, expr, rpar),
      (_,f,_,arg1,_,arg2,_) => if f.id == "pow" then -math.pow(arg1,arg2).toInt else 0)
    assertEquals(r7.fn(Token.Minus(), Token.FuncId("pow"), Token.LPar(), 2, Token.Comma(), 10, Token.RPar()), -1024)
  }
  test("Rule7 reduction") {
    val r7 = Rule7[Token.Minus,Token.FuncId,Token.LPar,Int,Token.Comma,Int,Token.RPar, Int](
      expr,
      Seq(minus, funcId, lpar, expr, comma, expr, rpar),
      (_,f,_,arg1,_,arg2,_) => if f.id == "pow" then -math.pow(arg1,arg2).toInt else 0)
    val stack: Seq[Any] = Seq(Token.Minus(), Token.FuncId("pow"), Token.LPar(), 2, Token.Comma(), 10, Token.RPar(), Token.Plus(), 2)
    val rstack: Seq[Any] = Seq(-1024, Token.Plus(), 2)
    assertEquals(r7.reduce(stack), rstack)
  }

  test("Rule8 definition") {
    val r8 = Rule8[Token.Minus,Token.IntLit,Token.Times,Token.LPar,Int,Token.Plus,Int,Token.RPar,Int](
      expr,
      Seq(minus, intLit, times, lpar, expr, plus, expr, rpar),
      (_,lit,_,_,exp1,_,exp2,_) => -lit.value * (exp1 + exp2))
    assertEquals(r8.fn(Token.Minus(),Token.IntLit(3),Token.Times(),Token.LPar(),8,Token.Plus(),10,Token.RPar()), -54)
  }
  test("Rule8 reduction") {
    val r8: Rule = Rule8[Token.Minus,Token.IntLit,Token.Times,Token.LPar,Int,Token.Plus,Int,Token.RPar,Int](
      expr,
      Seq(minus, intLit, times, lpar, expr, plus, expr, rpar),
      (_,lit,_,_,exp1,_,exp2,_) => -lit.value * (exp1 + exp2))
    val stack: Seq[Any] = Seq(Token.Minus(),Token.IntLit(3),Token.Times(),Token.LPar(),8,Token.Plus(),10,Token.RPar(),Token.Minus(),Token.IntLit(11832))
    val rstack: Seq[Any] = Seq(-54, Token.Minus(), Token.IntLit(11832))
    assertEquals(r8.reduce(stack), rstack)
  }
****/
}
