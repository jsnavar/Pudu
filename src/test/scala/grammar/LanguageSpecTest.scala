import pudu.grammar._

// Tests for rule definitions

class LanguageSpecTest extends munit.FunSuite {
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

  test("Rule1") {
    object SimpleArithmetic extends LanguageSpec[Int, Token]:
      // Symbol objects
      val expr = NonTerminal[Int]

      val lpar   = Terminal[Token.LPar]
      val rpar   = Terminal[Token.RPar]
      val plus   = Terminal[Token.Plus]
      val times  = Terminal[Token.Times]
      val minus  = Terminal[Token.Minus]
      val comma  = Terminal[Token.Comma]
      val funcId = Terminal[Token.FuncId]
      val intLit = Terminal[Token.IntLit]

      override val start = expr

      // Rules
      (expr ::= intLit) {
        _.value
      }

    val rules = SimpleArithmetic.rules
    assert(rules.size == 1)

    val firstRule = rules.head
    assertEquals(firstRule.left, SimpleArithmetic.expr)
    assertEquals(firstRule.right, Seq(SimpleArithmetic.intLit))

    val stack = Seq(Token.IntLit(10))
    assertEquals(firstRule.reduce(stack), Seq(10))
  }

  test("Rule2") {
    object SimpleArithmetic extends LanguageSpec[Int, Token]:
      // Symbol objects
      val expr = NonTerminal[Int]

      val lpar   = Terminal[Token.LPar]
      val rpar   = Terminal[Token.RPar]
      val plus   = Terminal[Token.Plus]
      val times  = Terminal[Token.Times]
      val minus  = Terminal[Token.Minus]
      val comma  = Terminal[Token.Comma]
      val funcId = Terminal[Token.FuncId]
      val intLit = Terminal[Token.IntLit]

      override val start = expr

      // Rules
      (expr ::= (plus, intLit)) {
        _._2.value
      }

    val rules = SimpleArithmetic.rules
    assert(rules.size == 1)

    val firstRule = rules.head
    assertEquals(firstRule.left, SimpleArithmetic.expr)
    assertEquals(firstRule.right, Seq(SimpleArithmetic.plus, SimpleArithmetic.intLit))

    val stack = Seq(Token.Plus(), Token.IntLit(10))
    assertEquals(firstRule.reduce(stack), Seq(10))
  }

  test("Rule3") {
    object SimpleArithmetic extends LanguageSpec[Int, Token]:
      // Symbol objects
      val expr = NonTerminal[Int]

      val lpar   = Terminal[Token.LPar]
      val rpar   = Terminal[Token.RPar]
      val plus   = Terminal[Token.Plus]
      val times  = Terminal[Token.Times]
      val minus  = Terminal[Token.Minus]
      val comma  = Terminal[Token.Comma]
      val funcId = Terminal[Token.FuncId]
      val intLit = Terminal[Token.IntLit]

      override val start = expr

      // Rules
      (expr ::= (expr, plus, expr)) {
        (exp1, _, exp2) => exp1 + exp2
      }

    val rules = SimpleArithmetic.rules
    assert(rules.size == 1)

    val firstRule = rules.head
    assertEquals(firstRule.left, SimpleArithmetic.expr)
    assertEquals(firstRule.right, Seq(SimpleArithmetic.expr, SimpleArithmetic.plus, SimpleArithmetic.expr))

    val stack = Seq(4, Token.Plus(), 4, 2)
    assertEquals(firstRule.reduce(stack), Seq(8, 2))
  }
  test("Rule4") {
    object SimpleArithmetic extends LanguageSpec[Int, Token]:
      // Symbol objects
      val expr = NonTerminal[Int]

      val lpar   = Terminal[Token.LPar]
      val rpar   = Terminal[Token.RPar]
      val plus   = Terminal[Token.Plus]
      val times  = Terminal[Token.Times]
      val minus  = Terminal[Token.Minus]
      val comma  = Terminal[Token.Comma]
      val funcId = Terminal[Token.FuncId]
      val intLit = Terminal[Token.IntLit]

      override val start = expr

      // Rules
      (expr ::= (minus, expr, times, expr)) {
        (_, exp1, _, exp2) => -exp1 * exp2
      }

    val rules = SimpleArithmetic.rules
    assert(rules.size == 1)

    val firstRule = rules.head
    assertEquals(firstRule.left, SimpleArithmetic.expr)
    assertEquals(firstRule.right, Seq(SimpleArithmetic.minus, SimpleArithmetic.expr, 
                                      SimpleArithmetic.times, SimpleArithmetic.expr))
    val stack = Seq(Token.Minus(), 4, Token.Times(), 4, 2)
    assertEquals(firstRule.reduce(stack), Seq(-16, 2))
  }

  test("Rule5") {
    object SimpleArithmetic extends LanguageSpec[Int, Token]:
      // Symbol objects
      val expr = NonTerminal[Int]

      val lpar   = Terminal[Token.LPar]
      val rpar   = Terminal[Token.RPar]
      val plus   = Terminal[Token.Plus]
      val times  = Terminal[Token.Times]
      val minus  = Terminal[Token.Minus]
      val comma  = Terminal[Token.Comma]
      val funcId = Terminal[Token.FuncId]
      val intLit = Terminal[Token.IntLit]

      override val start = expr

      // Rules
      (expr ::= (lpar, expr, minus, expr, rpar)) {
        (_, exp1, _, exp2, _) => exp1 - exp2
      }

    val rules = SimpleArithmetic.rules
    assert(rules.size == 1)

    import SimpleArithmetic._
    val firstRule = rules.head
    assertEquals(firstRule.left, expr)
    assertEquals(firstRule.right, Seq(lpar, expr, minus, expr, rpar))

    val stack = Seq(Token.LPar(), 4, Token.Minus(), 4, Token.RPar(), 2)
    assertEquals(firstRule.reduce(stack), Seq(0, 2))
  }

  test("Rule6") {
    object SimpleArithmetic extends LanguageSpec[Int, Token]:
      // Symbol objects
      val expr = NonTerminal[Int]

      val lpar   = Terminal[Token.LPar]
      val rpar   = Terminal[Token.RPar]
      val plus   = Terminal[Token.Plus]
      val times  = Terminal[Token.Times]
      val minus  = Terminal[Token.Minus]
      val comma  = Terminal[Token.Comma]
      val funcId = Terminal[Token.FuncId]
      val intLit = Terminal[Token.IntLit]

      override val start = expr

      // Rules
      (expr ::= (lpar, expr, minus, expr, rpar)) {
        (_, exp1, _, exp2, _) => exp1 - exp2
      }

      (expr ::= (funcId, lpar, expr, comma, expr, rpar)) {
        (fn, _, exp1, _, exp2, _) => if fn.id == "pow" then math.pow(exp1, exp2).toInt else 0
      }

    val rules = SimpleArithmetic.rules
    assert(rules.size == 2)

    import SimpleArithmetic._
    val funRule = rules.filter(_.right.head == funcId).head
    assertEquals(funRule.left, expr)
    assertEquals(funRule.right, Seq(funcId, lpar, expr, comma, expr, rpar))

    val stack = Seq(Token.FuncId("pow"), Token.LPar(), 4, Token.Comma(), 4, Token.RPar(), 2)
    assertEquals(funRule.reduce(stack), Seq(256, 2))
  }

  test("Rule7") {
    object SimpleArithmetic extends LanguageSpec[Int, Token]:
      // Symbol objects
      val expr = NonTerminal[Int]

      val lpar   = Terminal[Token.LPar]
      val rpar   = Terminal[Token.RPar]
      val plus   = Terminal[Token.Plus]
      val times  = Terminal[Token.Times]
      val minus  = Terminal[Token.Minus]
      val comma  = Terminal[Token.Comma]
      val funcId = Terminal[Token.FuncId]
      val intLit = Terminal[Token.IntLit]

      override val start = expr

      // Rules
      (expr ::= (lpar, expr, minus, expr, rpar)) {
        (_, exp1, _, exp2, _) => exp1 - exp2
      }

      (expr ::= (minus, funcId, lpar, expr, comma, expr, rpar)) {
        (_, fn, _, exp1, _, exp2, _) => if fn.id == "pow" then -math.pow(exp1, exp2).toInt else 0
      }

    val rules = SimpleArithmetic.rules
    assert(rules.size == 2)

    import SimpleArithmetic._
    val funRule = rules.filter(_.right(1) == funcId).head
    assertEquals(funRule.left, expr)
    assertEquals(funRule.right, Seq(minus, funcId, lpar, expr, comma, expr, rpar))

    val stack = Seq(Token.Minus(), Token.FuncId("pow"), Token.LPar(), 4, Token.Comma(), 4, Token.RPar(), 2)
    assertEquals(funRule.reduce(stack), Seq(-256, 2))
  }

  test("Rule8") {
    object SimpleArithmetic extends LanguageSpec[Int, Token]:
      // Symbol objects
      val expr = NonTerminal[Int]

      val lpar   = Terminal[Token.LPar]
      val rpar   = Terminal[Token.RPar]
      val plus   = Terminal[Token.Plus]
      val times  = Terminal[Token.Times]
      val minus  = Terminal[Token.Minus]
      val comma  = Terminal[Token.Comma]
      val funcId = Terminal[Token.FuncId]
      val intLit = Terminal[Token.IntLit]

      override val start = expr

      // Rules
      (expr ::= (lpar, expr, minus, expr, rpar)) {
        (_, exp1, _, exp2, _) => exp1 - exp2
      }

      (expr ::= (expr, minus, funcId, lpar, expr, comma, expr, rpar)) {
        (exp0, _, fn, _, exp1, _, exp2, _) => exp0 - (if fn.id == "pow" then math.pow(exp1, exp2).toInt else 0)
      }

    val rules = SimpleArithmetic.rules
    assert(rules.size == 2)

    import SimpleArithmetic._
    val funRule = rules.filter(_.right.head == expr).head
    assertEquals(funRule.left, expr)
    assertEquals(funRule.right, Seq(expr, minus, funcId, lpar, expr, comma, expr, rpar))

    val stack = Seq(1024, Token.Minus(), Token.FuncId("pow"), Token.LPar(), 4, Token.Comma(), 4, Token.RPar(), 2)
    assertEquals(funRule.reduce(stack), Seq(768, 2))
  }

  test("Several rules") {
    object SimpleArithmetic extends LanguageSpec[Int|List[Int], Token]:
      // Symbol objects
      val expr = NonTerminal[Int]
      val exprList = NonTerminal[List[Int]]

      val lpar   = Terminal[Token.LPar]
      val rpar   = Terminal[Token.RPar]
      val plus   = Terminal[Token.Plus]
      val times  = Terminal[Token.Times]
      val minus  = Terminal[Token.Minus]
      val comma  = Terminal[Token.Comma]
      val funcId = Terminal[Token.FuncId]
      val intLit = Terminal[Token.IntLit]

      override val start = expr
      
      (exprList ::= expr) { List(_) }
      (exprList ::= (expr, comma, exprList)) { (exp, _, list) => exp :: list }

      (expr ::= intLit) { _.value }
      (expr ::= (expr, plus, expr)) { t => t._1 + t._3 }
      (expr ::= (expr, minus, expr)) { (exp1,_,exp2) => exp1 - exp2 }
      (expr ::= (expr, times, expr)) { t => t._1 * t._3 }
      (expr ::= (funcId, lpar, exprList, rpar)) { (fn, _, args, _) => 
        fn.id match
          case "pow" =>
            require(args.size == 2)
            math.pow(args(0), args(1)).toInt
          case "max" => args.max
          case "min" => args.min
          case "sum" => args.sum
          case _ => 0
      }

    val rules = SimpleArithmetic.rules
    assert(rules.size == 7)
    assert(rules.filter(_.left == SimpleArithmetic.exprList).size == 2)
    assert(rules.filter(_.left == SimpleArithmetic.expr).size == 5)
    assertEquals(SimpleArithmetic.nonTerminals, Set(SimpleArithmetic.expr, SimpleArithmetic.exprList))
    assert(SimpleArithmetic.terminals.size == 8)
  }
}
