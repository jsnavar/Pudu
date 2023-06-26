# Pudu

[![CI](https://github.com/jsnavar/Pudu/actions/workflows/scala.yml/badge.svg)](https://github.com/jsnavar/Pudu/actions/workflows/scala.yml)

Shift-reduce parser and lexer generators in Scala 3, featuring type checked semantic actions, similar syntax for lexers and parsers, and a modular design with simple interfaces.

Pudu was modeled after [Flex](https://github.com/westes/flex) and [Bison](https://www.gnu.org/software/bison/), but implemented as a library instead of a separate program: all definitions —tokens, lexers, parsers and actions— are declared using standard Scala code. This simplifies the usage and development of the library, but comes with a few drawbacks in comparison with Flex and Bison. Maybe the most important one is that a program using Pudu includes the *specification* of the parser, but not the parser itself, which needs to be generated in each execution. Most of the times, this can be avoided using serialization, by saving the serialized parser to a file, and reading it back later, but that is not a very clean solution. Nevertheless, as Pudu is intended to be a learning tool, we do not concern ourselves with that problem.

A complete example application using Pudu can be found at [PuduCalc](https://github.com/jsnavar/PuduCalc).

## Distribution
Pudu is being distributed through [Github Packages](https://github.com/features/packages) and can be obtained using the [sbt-github-packages](https://github.com/djspiewak/sbt-github-packages) plugin. See PuduCalc  ([build.sbt](https://github.com/jsnavar/PuduCalc/blob/main/build.sbt)) for an example.

### Required options
As seen in the example, it is required to compile with the `"-Yretain-trees"` option. This may change in the future if we find a better solution to the following problem (suggestions welcome!):

Given a parameterized enum such as:
```scala
enum Example:
  case SomeCase(x: Int)
  case AnotherCase(y: String, z: Something)
```
How do we find the ordinal of a case using only its type (i.e., without an actual object)?. Here, the idea is to have a function `enumOrdinal[T]` such that `enumOrdinal[Example.SomeCase] == 0`, and `enumOrdinal[Example.AnotherCase] == 1`.

### Giter8 template
A [giter8](https://github.com/foundweekends/giter8) template is provided to simplify the creation of new projects. To use it, just run:
```
sbt new jsnavar/Pudu.g8
```

## Usage

Representing a language with Pudu requires three elements: An enum of tokens, a lexer specification, and a grammar specification:

### Tokens
For technical reasons, tokens must be declared using an enum with parameterized cases. This enum also needs to include two special cases: one for end of file and another for lexical errors. There are no restrictions on these tokens, as they are declared later in the lexer and parser.

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

Optionally, tokens can also include positions, which are defined as parameters with name `p`. Pudu checks and gets the value of `p` using reflection, mostly to generate error messages. The previous example with positions is:

```scala
case class Position(line: Int, column: Int, length: Int)

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

Specifying a lexer requires to list regular expressions paired with *operations*, which can be of two kinds: to generate a token, or to ignore the matched string. Given a regular expression as a `String`, the operations are declared with the following extension methods:
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
  "," { _ => Token.Comma() }
```
There, the regex "," only matches the string ",", so the function always returns the same and does not really use the matched string. Considering this, we add method (2) which allows us to write:
```scala
  "," { Token.Comma() }
```
Method (3) simply ignores the matched string, and method (4) calls `fn` on the ignored string, which can be useful for side effects such as updating positions.

As anticipated before, the lexer needs to know the end of file token, which is declared by overriding the method `def eof: Token`. This method is called automatically when the input ends, so it is not necessary to add an explicit rule for it.

Continuing the previous example, the lexer corresponding to `Token` could be:
```scala
object ArithmeticLexer extends Lexer[Token]:
  "\\(" { Token.LPar() }
  "\\)" { Token.RPar() }
  "\\+" { Token.Plus() }
  "\\*" { Token.Times() }
  "\\-" { Token.Minus() }
  "," { Token.Comma() }
  "[a-z][a-z0-9]*" { Token.Id(_) }
  "-?[0-9]+" { s => Token.IntLiteral(s.toInt) }

  "[ \n\t]+".ignore
  "." { Token.ERROR() }

  override val eof = Token.EOF()
```

**About Lexer Semantics:**
Given a string and a set of regular expressions, there may be more than one tokenization of the input. For a common example, consider the string `"2-1"` which can be tokenized as `(IntLiteral(2), Minus, IntLiteral(1))`, or `(IntLiteral(2), IntLiteral(-1))`. This opens the problem of how to choose the *right* tokenization, which can not be solved by the lexer itself as the answer depends on the grammar: in many languages, the first tokenization of the example is a valid expression while the second is not. Taking this into account, we follow the tradition of lex and flex, and use a greedy approach, where in each iteration, the lexer grabs the longest prefix that is matched by a regex, continuing until the string ends, or no match is found. In both cases, Pudu marks the end with the eof token.

Resuming the example, the string `"abcd123+10-1"` would be tokenized as:
```scala
(Id("abcd123"), Plus(), IntLiteral(10), IntLiteral(-1), EOF())
```

It is recommended to avoid the case where the lexer does not find a match, by generating the Error token on unmatched characters. To do so, we recommend using a catch-all single character regex, like in the example:
```scala
  "." { Token.ERROR() }
```
Another consideration is that more than one regex may match the longest prefix, for example keywords and identifiers in most programming languages. In this case, we also follow tradition and choose the expression declared first: in the example, the string "1" would match as an IntLiteral and not as an ERROR. 

In summary, it is recommended to add the single character regex `"."`, generating an error, as the last entry of the specification.

### Language Specification
Before generating a parser, we need to specify the grammar augmented with semantic actions. In Pudu, this is done in a separate class in order to facilitate a modular design, where the same grammar can be used for different parser generators. This simplifies the implementation of new generators, and also allows the users to compare different parsers of the same language.

To specify a language, we need to extend `pudu.grammar.LanguageSpec[Tree, Token]`, which takes two type parameters: Tree and Token. `Tree` is a supertype of all the values generated by semantic actions, for example, the top type of an ADT, an union type, or just `Any`; and `Token` is the same enum used by the lexer, as defined above.

A language specification contains several elements:
- Grammar symbols: There must be one term -usually a val- for each terminal and non terminal in the grammar. These values are declared using the `Terminal[+T <: reflect.Enum]` and `NonTerminal[T]` classes. For example, in:
```scala
val id = Terminal[Token.Id]
val expr = NonTerminal[Expr[Int]]
```
we declare two symbols, a terminal `id` corresponding to the token `Token.Id`, and a non terminal `expr`, with semantic type `Expr[Int]`. It is also possible and recommended to add names to symbols, using the overloaded apply method: `val expr = NonTerminal[Expr[Int]]("expr")`. In this case, error messages and parser reports would use the string "expr" to refer to that symbol, instead of a synthetic name, which can be hard to read.

- Special symbols: An specification must override three special symbols: `eof: Terminal[Token]`, `error: Terminal[Token]` and `start: NonTerminal[Tree]`, which denote end of file, lexical errors, and the start symbol of the grammar.
```scala
  override val eof = Terminal[Token.EOF]
  override val error = Terminal[Token.ERROR]
  override val start = expr
```
It is very important to mention that `eof` and  `error` must *not* be used in the grammar, as they are handled in a special way.

- Operator precedence: This is optional, and done overriding the `precedence` method:
```scala
  override val precedence = Precedence()
    .left(plus, minus)
    .left(times, div)
    .nonassoc(uminus, uplus)
    .right(caret)
```
As usual, precedence definitions consist of several levels of precedence, each of them with an associativity rule, which can be left, right or nonassoc. Calling `Precedence()` returns an empty definition, and varargs methods `left`, `right` and `nonassoc` generate a new Precedence object with an added level of the corresponding kind.

- Productions: Finally, we need the grammar productions. Here we use the method `::=`, which is an extension method of `NonTerminal[T <: Tree]`. It has two parameters in separate parameter lists: a NonEmptyTuple of symbols (Terminals or NonTerminals), and a function representing the semantic action. This function's type is inferred from the grammar symbols, for example, in:
```scala
enum Token:
  case Literal(value: Int)
  case Plus()
  ...
object Calc extends LanguageSpec[Int, Token]:
  val expr = NonTerminal[Int]("expr")
  val plus = Terminal[Token.Plus]
  val literal = Terminal[Token.Literal]
  ...
  (expr := (literal, plus, expr)) { fn }
```
`fn` must conform with type `(Token.Literal, Token.Plus, Int) => Int`. Usually, this function is anonymous, so the last line could be implemented as:
```scala
  (expr := (literal, plus, expr)) { (lit, _, num) => lit.value + num }
```

### Parsing
Now, we have all the ingredients needed to generate a parser.

First, we get the lexer function from the lexer object:
```scala
  val lexer: String => Iterator[Token] = ArithmeticLexer.lexer
```
then, we create a parser generator, and get the parser:
```scala
  val parserGen = SLRParserGenerator(Calc.grammar)
  val parser: Iterator[Token] => Either[ErrorMsg, Tree] = parserGen.parser
```
and finally, we can compose both into a single function, or simply nest them:
```scala
  val strParser = parser.compose(lexer)
  val result = strParser("2 * 3 - 4") // = Right(2)
```

### Reporting
Pudu also includes a class `pudu.parser.generator.LRReport`, which generates string representations of parser generators. For example:
```scala
  val parserGen = SLRParserGenerator(Calc)
  val report = parserGen.report

  println(reporter.actionTable)
```
would print the actions table. LRReport also contains methods to print the productions, states, LR0 automaton, and goto table.

## Code Organization

There are three packages:
- pudu.grammar
- pudu.lexer
- pudu.parser

### pudu.grammar package
This package contains the code needed to transform a language specification to the internal representation used by parser generators. Main classes are:
- pudu.grammar.Symbol: grammar symbols: Terminal[Tree] and NonTerminal[Token]
- pudu.grammar.Precedence: Precedence definitions
- pudu.grammar.LanguageSpec: Language specification
- pudu.grammar.Grammar: Grammar derived from a LanguageSpec.
- pudu.grammar.Rule: Internal representation of productions

### pudu.lexer
As the name implies, this is the lexer implementation.

### pudu.parser
Contains the parser generators. For now, that is SLR and LR(1) (using the standard algorithm).

## Writing a custom parser or parser generator
To implement a parser generator, it is recommended to use a `Grammar[Tree, Token]` object as parameter, which exposes:
- `rules: Set[Rule[Tree, Token]]`: Set of productions, where `Rule[Tree, Token]` is defined as:
```scala
  case class Rule[Tree, Token](left: Symbol,
                               right: Seq[Symbol],
                               action: Seq[Tree|Token] => Tree)):
```
- `terminals: Set[Symbol]`: Set of terminals.
- `nonTerminals: Set[Symbol]`: Set of non terminals.
- `startSymbol: NonTerminal[Tree]`: start symbol of the grammar.
- `eof: Terminal[Token]`: end of file terminal.
- `error: Terminal[Token]`: lexical error terminal.
- `precedence: Precedence`: operator precedence rules.
- `augmented`: Grammar[Tree, Token]: method that generates a new grammar, augmented with a new start symbol.
- `terminalNames`: Map[Int, String]: maps token ordinals to terminal names.

The package `pudu.parser.generator`, contains implementations of methods common to LR parsers. For example, first and follow are implemented in `LRFirstFollow`, the construction of the automaton is in `LRAutomaton`, and so on. For an example, see `pudu.parser.generator.SLRParserGenerator`.
