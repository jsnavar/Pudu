# Pudu

Shift-reduce parser and lexer generator in Scala 3.

It is mostly inspired in flex and bison, but used as a library where all definitions —tokens, lexer, parser and actions— are specified using standard Scala code. This simplifies the usage of the library, but comes with a few drawbacks in comparison with bison and flex. Maybe the most important disadvantage is that a program using Pudu includes the *specification* of the parser, but not the parser itself, which needs to be generated in each execution. Most times, this can be avoided using serialization, by saving the serialized parser to a file, and reading it back later, but that is not a very clean solution. Nevertheless, as Pudu is a learning tool (for me), we do not concern ourselves with that problem.

## Usage

Representing a language with Pudu requires three elements: An enum of tokens, a lexer specification, and a grammar specification:

### Tokens
For technical reasons, tokens must be declared using an enum with parameterized cases. This enum also needs to include two special cases: one for end of file and another for lexical errors. There are no restrictions about eof or error tokens, as they are declared later in the lexer and parser.

For example, an enum of tokens for arithmetical operations could be:

```scala
enum Token:
  case LPar()
  case RPar()
  case Times()
  case Plus()
  case Minus()
  case Id(name: String)
  case IntLiteral(value: Int)
  case Comma()
  case EOF()
  case ERROR()
```

Optionally, tokens can also include positions, which are defined as parameters of name `p`. Pudu checks and gets the value of `p` using reflection, mostly to generate error messages. The previous example with positions would be:

```scala
case class Position(line: Int, column: Int)

enum Token:
  case LPar(p: Position)
  case RPar(p: Position)
  case Times(p: Position)
  case Plus(p: Position)
  case Minus(p: Position)
  case Id(name: String, p: Position)
  case IntLiteral(value: Int, p: Position)
  case Comma(p: Position)
  case EOF(p: Position)
  case ERROR(p: Position)
```
### Lexer

Specifying a lexer requires to list regular expressions paired with *operations*, which can be of two kinds: generate a token or ignore the matched string. Given a regular expression pattern as a `String`, the operations mentioned above are described using the following extension methods:
```scala
  extension (pattern: String)
    protected def apply(fn: String => Token) = ... // (1)
    protected def apply(token: Token) = ...        // (2)
    protected def ignore = ...                     // (3)
    protected def ignore(fn: String => Unit) = ... // (4)
```
In method (1), the parameter `fn` takes the matched string and returns the corresponding token. For example:
```scala
  "[0-9]+" { s => Token.IntLiteral(s.toInt) }
```
Sometimes, the token does not depend on the matched string, so as syntactic sugar we add method (2), which is equivalent to call method (1) with parameter `_ => token`. One example of this is:
```scala
  "\\+" { Token.Plus() }
```
Method (3) simply ignores the matched string, and method (4) calls `fn` on the string, which can be used for side effects such as updating positions.

As mentioned above, the lexer needs to specify the end of file token, overriding the method `def eof: Token`. It is important to note that the lexer generates that token automatically when the input ends, and it shoult **NOT** be returned by any regular expression.

Continuing the previous example, the lexer corresponding to `Token` could be:
```scala
object ArithmeticLexer extends Lexer[Token]:
  "\\(" { Token.LPar() }
  "\\)" { Token.RPar() }
  "\\+" { Token.Plus() }
  "\\*" { Token.Times() }
  "\\-" { Token.Minus() }
  "\\," { Token.Comma() }
  "[a-z]+[a-z0-9]*" { Token.Id(_) }
  "[0-9]+" { s => Token.IntLiteral(s.toInt) }

  "[ \n\t]+".ignore
  "." { Token.ERROR(_) }

  override val eof = Token.EOF()
```
Tokenizing an input string is done with the `lexer` method, which takes a `CharSequence`, and returns an `Iterator[Token]`:
```scala
  val tokensIterator = ArithmeticLexer.lexer("2 + 3")
  // tokensIterator.toList = List(Token.IntLiteral(2), Token.Plus(), Token.IntLiteral(3), Token.EOF())
```

**About Lexer Semantics:**
Given a string and a set of regular expressions, there may be more than one way of tokenizing the input. As a simple example consider the regex "[0-9]+", which can tokenize the string "123" as ("123"), ("12", "3"), ("1", "23"), and ("1", "2", "3"). This opens the problem of choosing the *right* tokenization, which does not have any reasonable  definition in general, but nonetheless we need to generate a single tokenization.

In this context, we follow the tradition of lex, flex and similar lexer generators, and use a greedy approach. Given a string, we choose the regex that matches the longest prefix, call the corresponding action (generate a token or ignore), and continue recursively with the rest of the string. This process continues until the input ends, or no regex match a prefix. If the input ends, the lexer generates the eof token by calling the eof method, and if there are no prefix matches, the process stops without generating an eof. It is advised to avoid this later case, by using a catch-all error regex, like in the example:
```scala
  "." { Token.ERROR(_) }
```
Another consideration is that more than one regex may match the longest prefix, in the example for string "2 + 3", the prefix "2" is matched by `"[0-9]+"` and by `"."`. In this case, also following tradition, we choose the expression defined first. In summary, it is recommended to add a single character regex `"."`, that generate an error, as the last entry of the specification.

### Parser specification


### Parsing


## Code Organization

There are three packages:
- pudu.grammar
- pudu.lexer
- pudu.parser

Most of the time, there are references to two type parameters: Tree and Token <: scala.reflect.enum. Tree refers to the AST type, 

### pudu.grammar package

This package contains the code needed to transform a grammar specification to the internal representation used by parser generators. Main classes are:
- pudu.grammar.Symbol: ADT for grammar symbols, i.e. Terminal[Tree] and NonTerminal[Token]
