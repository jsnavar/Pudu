package pudu.grammar

/** Transforms a curried function 'f' of type T1 => T2 => ... => Tn => R into
 *  function calls equivalent to (omitting the asInstanceOf for readability):
 {{{
   (args: Seq[ST]) => f(args(0))(args(1))(args(2))...
 }}}
 * In the future, the return type should be simplified to Seq[Any] => R, ommiting
 * the parameter ST. This should be possible using a recursive match type similar
 * to:
 {{{
 type Return[X] = X match
   case Function1[_,t] => Return[t]
   case _ => X
 }}}
 */
inline def seq[ST, S, R](inline numPars: Int, inline fn: S => R): Seq[ST] => ST = seq(numPars, numPars-1, fn)
inline def seq[ST, S, R](inline numPars: Int, inline depth: Int, inline fn: S => R): Seq[ST] => ST =
  (args: Seq[Any]) =>
    inline if depth == 0 then
      fn(args(depth).asInstanceOf[S]).asInstanceOf[ST]
    else
      inline fn(args(depth).asInstanceOf[S]) match
        case nextFn: Function1[_,_] =>
          seq(numPars, depth-1, nextFn)(args).asInstanceOf[ST]

/** Gets the ordinal value of an enum case from it's type.
 *  This macro abuses the implementation of Enums, using that
 *  when cases are parametrized, 'ordinal' is implemented as
 *  def ordinal = IntConstant //Actually a Literal with an IntConstant */
import scala.quoted.*
inline def enumOrdinal[T] = ${ enumOrdinalImpl[T] }

def enumOrdinalImpl[T: Type](using Quotes) =
  import quotes.reflect.*
  val ts = TypeRepr.of[T].typeSymbol
  val ordinalSymbol = ts.methodMember("ordinal").head
  // this requires the scalac option -Yretain-trees
  val rhs = ordinalSymbol.tree.asInstanceOf[DefDef].rhs
  rhs match
    case None => throw Exception("Tree not found. Please compile with option -Yretain-trees")
    case Some(x) => x.asExprOf[Int]

inline def untupled[T1, T2, R] = Function.untupled[T1, T2, R]
inline def untupled[T1, T2, T3, R] = Function.untupled[T1, T2, T3, R]
inline def untupled[T1, T2, T3, T4, R] = Function.untupled[T1, T2, T3, T4, R]
inline def untupled[T1, T2, T3, T4, T5, R] = Function.untupled[T1, T2, T3, T4, T5, R]

inline def untupled[T1, T2, T3, T4, T5, T6, R](fn: ((T1, T2, T3, T4, T5, T6)) => R): (T1, T2, T3, T4, T5, T6) => R = fn(_, _, _, _, _, _)
inline def untupled[T1, T2, T3, T4, T5, T6, T7, R](fn: ((T1, T2, T3, T4, T5, T6, T7)) => R): (T1, T2, T3, T4, T5, T6, T7) => R = fn(_, _, _, _, _, _, _)
inline def untupled[T1, T2, T3, T4, T5, T6, T7, T8, R](fn: ((T1, T2, T3, T4, T5, T6, T7, T8)) => R): (T1, T2, T3, T4, T5, T6, T7, T8) => R = fn(_, _, _, _, _, _, _, _)
