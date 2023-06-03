package pudu.grammar

/* Given a function fn: Tup => ST, where Size[Tup] is equal to size, it returns a function Seq[ST] => ST,
 * which takes the first 'size' elements of the sequence, reverse them, cast them to a tuple Tup (!!),
 * and finally calls fn on that tuple.
 * By itself this is obviously unsafe, but used in reductions of a 'correctly implemented' shift
 * reduce parser, where the semantic actions were defined using the ::= extension method, then it is safe.
 * This can be proved using that actions in ::= are type checked: each argument match the SymData of the
 * corresponding symbol, and the return type matches the SymData of the NonTerminal in the lhs; and when the
 * parser reduces by a production, then the top of the stack has elements corresponding to the rhs of that production. */
inline def toSeqFn[ArgsType, RetType, Tup <: Tuple](inline size: Int, inline fn: Tup => RetType): Seq[ArgsType] => RetType =
  (args: Seq[ArgsType]) =>
    val top = args.take(size).reverse.toArray[Any]
    val fnArgs = Tuple.fromArray(top).asInstanceOf[Tup]
    fn(fnArgs)

/* special case for unit productions */
inline def toSeqFn[ArgsType, RetType, T](inline fn: T => RetType): Seq[ArgsType] => RetType =
  (args: Seq[ArgsType]) =>
    fn(args.head.asInstanceOf[T])

/** Gets the ordinal value and name of an enum case from its type.
 *  This macro abuses the implementation of Enums, using that
 *  when cases are parameterized, 'ordinal' is implemented as
 *  def ordinal = IntConstant //Actually a Literal with an IntConstant */
import scala.quoted.*
inline def enumMetadata[T] = ${ enumMetadataImpl[T] }

def enumMetadataImpl[T: Type](using Quotes) =
  import quotes.reflect.*
  val ts = TypeRepr.of[T].typeSymbol
  val name = Expr(ts.name)
  val ordinalSymbol = ts.methodMember("ordinal").head
  // this requires the scalac option -Yretain-trees
  val rhs = ordinalSymbol.tree.asInstanceOf[DefDef].rhs
  val ordinal = rhs match
    case None => throw Exception("Tree not found. Please compile with option -Yretain-trees")
    case Some(x) => x.asExprOf[Int]
  Expr.ofTuple(ordinal, name)
