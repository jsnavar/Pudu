import pudu.grammar._

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
  val eofTerminal = Terminal[Token.EOF]

  override val eof = eofTerminal
  override val start = expr

  (exprList ::= expr) { List(_) }
  (exprList ::= (expr, comma, exprList)) { (exp, _, list) => exp :: list }

  (expr ::= intLit) { _.value }
  (expr ::= (lpar, expr, rpar)) { _._2 }
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
