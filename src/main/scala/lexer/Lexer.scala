package pudu.lexer

import scala.util.matching._

/** Simple lexer based on [[scala.util.matching.Regex]] */
abstract class Lexer[Token <: reflect.Enum]:
  /* Regular expressions can generate a Token or be ignored (whitespace, comments, ...).
   * This is represented with two case classes: TokenCase and IgnoreCase. */
  sealed trait LexerCase:
    def regex: Regex
  case class TokenCase(regex: Regex, fn: String => Token) extends LexerCase
  /* The function in IgnoreCase can be used for side effects, like printing/logging info,
   * or updating position data */
  case class IgnoreCase(regex: Regex, fn: String => Unit) extends LexerCase

  case class Match(result: Regex.Match, entry: LexerCase)

  private var entries = Seq.empty[LexerCase]

  def eof: Token

  /* Register the LexerCases into 'entries' */
  extension (str: String)
    // TokenCase with explicit function
    protected def apply(fn: String => Token) =
      entries = entries :+ TokenCase(str.r, fn)
    // TokenCase with constant function
    protected def apply(token: Token) =
      entries = entries :+ TokenCase(str.r, Function.const(token))
    // IgnoreCase with explicit function
    protected def ignore(fn: String => Unit) =
      entries = entries :+ IgnoreCase(str.r, fn)
    // IgnoreCase with constant function
    protected def ignore =
      entries = entries :+ IgnoreCase(str.r, Function.const(()))

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

  /** Returns the next token, by calling the previous method until finding
   *  a TokenCase, and then using TokenCase.fn to generate the Token instance */
  private def nextToken(input: CharSequence): Option[(Token, CharSequence)] =
    nextMatch(input).flatMap{ 
      case Match(result, TokenCase(_, fn)) =>
        Some((fn(result.matched), result.after))
      case Match(result, IgnoreCase(_, fn)) =>
        fn(result.matched)
        nextToken(result.after)
    }

  def lexer(input: CharSequence): Iterator[Token] =
    var remainder = input
    val first = new Iterator[Token] {
      var curToken = nextToken(input)
      def hasNext = remainder.length() > 0 && curToken.isDefined

      def next(): Token =
        val (token, after) = curToken.get
        remainder = after
        curToken = nextToken(remainder)
        token
    }
    def endOfFile = 
      if remainder.length == 0 then Seq(eof)
      else Seq.empty[Token]

    first.concat(endOfFile)
