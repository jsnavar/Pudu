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

  /* method called on end of file. It is a def because the EOF token could depend
   * on the state of the lexer (for example, for position data) */
  protected def eof: Token

  /* Register the LexerCases into 'entries' */
  extension (pattern: String)
    protected def apply(fn: String => Token) =
      entries = entries :+ TokenCase(pattern.r, fn)
    protected def apply(token: Token) =
      entries = entries :+ TokenCase(pattern.r, _ => token)
    protected def ignore(fn: String => Unit) =
      entries = entries :+ IgnoreCase(pattern.r, fn)
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
    val firstMatch = nextMatch(input)
    val iter: Option[Match] => Option[Match] =
      _.flatMap(m => nextMatch(m.result.after))

    Iterator.iterate(firstMatch)(iter)
      .takeWhile(_.isDefined)
      .collect {
        case Some(Match(m, TokenCase(_, fn))) => Some(fn(m.matched))
        case Some(Match(m, IgnoreCase(_, fn))) => fn(m.matched) ; None }
      .collect { case Some(tok) => tok }
      .concat(Iterator.single(eof))
