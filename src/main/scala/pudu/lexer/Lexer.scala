package pudu.lexer

import scala.util.matching._

/** Simple lexer based on [[scala.util.matching.Regex]] */
abstract class Lexer[Token <: reflect.Enum]:
  /* Regular expressions can generate a Token or be ignored (whitespace, comments, ...).
   * This is represented with two case classes: TokenCase and IgnoreCase. */
  private sealed trait LexerCase:
    def regex: Regex
  private case class TokenCase(regex: Regex, fn: String => Token) extends LexerCase
  /* The function in IgnoreCase can be used for side effects, like printing/logging info,
   * or updating position data */
  private case class IgnoreCase(regex: Regex, fn: String => Unit) extends LexerCase

  private var entries = Seq.empty[LexerCase]

  protected def eof: Token

  /* Register the LexerCases into 'entries' */
  extension (pattern: String)
    // TokenCase with explicit function
    protected def apply(fn: String => Token) =
      entries = entries :+ TokenCase(pattern.r, fn)
    // TokenCase with constant function
    protected def apply(token: Token) =
      entries = entries :+ TokenCase(pattern.r, _ => token)
    // IgnoreCase with explicit function
    protected def ignore(fn: String => Unit) =
      entries = entries :+ IgnoreCase(pattern.r, fn)
    // IgnoreCase with constant function
    protected def ignore =
      entries = entries :+ IgnoreCase(pattern.r, _ => ())

  /* Internal representation of a match. */
  private case class Match(result: Regex.Match, entry: LexerCase)

  /** Returns the next prefix Match.
   *  If there is more than one, it returns the longest, and if
   *  there is more than one with max length, which means that at
   *  least two expressions matched the same prefix, it returns the
   *  one defined first. */
  private def nextMatch(input: CharSequence): Option[Match] =
    entries.map(_.regex.findPrefixMatchOf(input))
      .zip(entries)
      .collect { case (Some(x), entry) => Match(x, entry) }
      .maxByOption(_.result.end)

  def lexer(input: CharSequence): Iterator[Token] =
    Iterator.iterate(nextMatch(input)) { _.flatMap {
      case Match(m, _) => nextMatch(m.after) }} // iterator of Option[Match] with an infinite tail of 'None's
        .takeWhile(_.isDefined)
        .collect {
          // for each match, generate a Some(Token) if it is a TokenCase,
          // or None (after calling the side effects function) if it is an IgnoreCase
          case Some(Match(m, TokenCase(_, fn))) => Some(fn(m.matched))
          case Some(Match(m, IgnoreCase(_, fn))) => fn(m.matched) ; None }
        .collect { case Some(tok) => tok }
        .concat(Iterator.single(eof))
