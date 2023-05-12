import pudu.grammar._
import pudu.lexer._

enum Token:
  case LPar() //0
  case RPar() //1
  case Plus() //2
  case Times()  //3
  case Minus()  //4
  case Comma()
  case FuncId(id: String)
  case IntLit(value: Int)
  case EOF()
  case ERROR(str: String)

object SimpleArithmetic extends LanguageSpec[Int|List[Int], Token]:
  // Symbol objects
  val expr = NonTerminal[Int]("Expr")
  val exprList = NonTerminal[List[Int]]("ExprList")

  val lpar   = Terminal[Token.LPar]
  val rpar   = Terminal[Token.RPar]
  val plus   = Terminal[Token.Plus]
  val times  = Terminal[Token.Times]
  val minus  = Terminal[Token.Minus]
  val comma  = Terminal[Token.Comma]
  val funcId = Terminal[Token.FuncId]
  val intLit = Terminal[Token.IntLit]

  override val eof = Terminal[Token.EOF]
  override val error = Terminal[Token.ERROR]
  override val start = expr
  override val precedence = Precedence().left(plus, minus).left(times)

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

object ArithmeticLexer extends Lexer[Token]:
  "\\(" { Token.LPar() }
  "\\)" { Token.RPar() }
  "\\+" { Token.Plus() }
  "\\*" { Token.Times() }
  "\\-" { Token.Minus() }
  "\\," { Token.Comma() }
  "[a-z]+[a-z0-9]*" { Token.FuncId(_) }
  "[0-9]+" { s => Token.IntLit(s.toInt) }

  "[ \n\t]+".ignore
  "." { Token.ERROR(_) }

  override val eof = Token.EOF()
