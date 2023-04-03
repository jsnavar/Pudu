package pudu.grammar

/** Transforms a curried function of type T1 => T2 => ... => Tn => R into a function
 *  of type Seq[Any] => Any, which uses the first n elements from the Seq as arguments */
inline def seq[S, R](inline depth: Int, inline fn: S => R): Seq[Any] => Any =
  (args: Seq[Any]) =>
    if depth == 1 then fn(args.head.asInstanceOf[S])
    else
      fn(args.head.asInstanceOf[S]) match
        case nextFn: Function1[_,_] =>
          seq(depth-1, nextFn)(args.tail)

inline def stackFunction[S, R](inline depth: Int, inline fn: S => R): Seq[Any] => Seq[Any] =
  (stack: Seq[Any]) =>
    val (top, bottom) = stack.splitAt(depth)
    val result = seq(depth, fn)(top)
    result +: bottom

inline def untupled[T1, T2, R] = Function.untupled[T1, T2, R]
inline def untupled[T1, T2, T3, R] = Function.untupled[T1, T2, T3, R]
inline def untupled[T1, T2, T3, T4, R] = Function.untupled[T1, T2, T3, T4, R]
inline def untupled[T1, T2, T3, T4, T5, R] = Function.untupled[T1, T2, T3, T4, T5, R]

inline def untupled[T1, T2, T3, T4, T5, T6, R](fn: ((T1, T2, T3, T4, T5, T6)) => R): (T1, T2, T3, T4, T5, T6) => R = fn(_, _, _, _, _, _)
inline def untupled[T1, T2, T3, T4, T5, T6, T7, R](fn: ((T1, T2, T3, T4, T5, T6, T7)) => R): (T1, T2, T3, T4, T5, T6, T7) => R = fn(_, _, _, _, _, _, _)
inline def untupled[T1, T2, T3, T4, T5, T6, T7, T8, R](fn: ((T1, T2, T3, T4, T5, T6, T7, T8)) => R): (T1, T2, T3, T4, T5, T6, T7, T8) => R = fn(_, _, _, _, _, _, _, _)
