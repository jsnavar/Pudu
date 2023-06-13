package pudu.grammar

import scala.quoted.*

/* Given a function fn: Tup => ST, where Tup is a tuple type of arity 'size',
 * toSeqFnTuple(fn) is inlined to the function:
 * {{{
     (x: Seq[Args]) => fn((x(size - 1), x(size - 2), ..., x(1), x(0)).asInstanceOf[Tup])
 * }}},
 * that corresponds to calling 'fn' on the top 'size' elements of the stack, casted to Tup (!!) in
 * reverse order (order is reversed because it is an stack).
 * By itself this is obviously unsafe, but used in reductions of a 'correctly implemented' shift
 * reduce parser, where the semantic actions were defined using the ::= extension method, then it is safe.
 * This can be proved using that actions in ::= are type checked: each argument match the SymData of the
 * corresponding symbol, and the return type matches the SymData of the NonTerminal in the lhs; and when the
 * parser reduces by a production, then the top of the stack has elements corresponding to the rhs of that production. */
inline def toSeqFnTuple[Args, Ret, Tup <: Tuple](inline fn: Tup => Ret): Seq[Args] => Ret =
  ${ toSeqFnTupleImpl('{compiletime.constValue[Tuple.Size[Tup]]}, 'fn) }

def toSeqFnTupleImpl[Args: Type, Ret: Type, Tup <: Tuple: Type](size: Expr[Int], fn: Expr[Tup => Ret])(using Quotes) =
  val idxSeq = (0 until size.valueOrAbort).reverse.map(Expr.apply)
  '{ (x: Seq[Args]) =>
    ${
      val argsExp = Expr.ofTupleFromSeq(idxSeq.map(i => '{x($i)}))
      '{ $fn(${argsExp}.asInstanceOf[Tup]) }
    }
  }

/* special case for unit productions */
inline def toSeqFn[ArgsType, RetType, T](inline fn: T => RetType): Seq[ArgsType] => RetType =
  (args: Seq[ArgsType]) =>
    fn(args.head.asInstanceOf[T])

/** Gets the ordinal value and name of an enum case from its type.
 *  This macro abuses the implementation of Enums, using that
 *  when cases are parameterized, 'ordinal' is implemented as
 *  def ordinal = IntConstant //Actually a Literal with an IntConstant */
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
