# Pudu

[![CI](https://github.com/jsnavar/Pudu/actions/workflows/scala.yml/badge.svg)](https://github.com/jsnavar/Pudu/actions/workflows/scala.yml)

Shift-reduce parser and lexer generator in Scala 3. It features type checked semantic actions, similar syntax for lexers and parsers, and a modular design.

It is mostly inspired in flex and bison, but used as a library where all definitions —tokens, lexers, parsers and actions— are specified using standard Scala code. This simplifies the usage of the library, but comes with a few drawbacks in comparison with bison and flex. Maybe the most important one is that a program using Pudu includes the *specification* of the parser, but not the parser itself, which needs to be generated in each execution. Most of the times, this can be avoided using serialization, by saving the serialized parser to a file, and reading it back later, but that is not a very clean solution. Nevertheless, as Pudu is intended to be a learning tool, we do not concern ourselves with that problem.

## Distribution
Pudu is available through [Github Packages](https://github.com/features/packages). Using sbt, it can be consumed with the [djspiewak/sbt-github-packages](https://github.com/djspiewak/sbt-github-packages) plugin, by adding `Resolver.githubPackages("jsnavar")` to `resolvers`, and `"pudu" %% "pudu" % "0.1"` to `libraryDependencies`. It is also required to add `"-Yretain-trees"` to `scalacOptions`, for reasons described [here](https://github.com/jsnavar/Pudu/blob/main/src/main/scala/grammar/functions.scala).

As a simpler alternative, we also provide a giter8 template, which creates an empty project with the right options. To use it, just execute:
```
sbt new jsnavar/Pudu.g8
```

## Usage

Representing a language with Pudu requires three elements: An enum of tokens, a lexer specification, and a grammar specification:

### Tokens
For technical reasons, tokens must be declared using an enum with parameterized cases. This enum also needs to include two special cases: one for end of file and another for lexical errors. There are no restrictions on these special tokens, as they are declared later in the lexer and parser.

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

Optionally, tokens can also include positions, which are defined as parameters with name `p`. Pudu checks and gets the value of `p` using reflection, mostly to generate error messages. The previous example with positions could be:

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

Specifying a lexer requires to list regular expressions paired with *operations*, which can be of two kinds: to generate a token or to ignore the matched string. Given a regular expression as a `String`, the operations are declared using the following extension methods:
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
In the example, the regex "," only matches the string ",", so the function always returns the same, and does not really use the matched string. Considering this, we add method (2) which allows us to write:
```scala
  "," { Token.Comma() }
```
Method (3) simply ignores the matched string, and method (4) calls `fn` on the ignored string, which can be used for side effects such as updating positions.

As anticipated above, the lexer needs to know the end of file token, which is declared by overriding the method `def eof: Token`. It is important to note that the lexer generates that token automatically when the input ends, and it should **NOT** be returned by any regular expression.

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

**About Lexer Semantics:**
Given a string and a set of regular expressions, there may be more than one way of tokenizing the input. As a simple example consider the regex "[0-9]+", which can tokenize the string "123" as ("123"), ("12", "3"), ("1", "23"), and ("1", "2", "3"). This opens the problem of how to choose the *right* tokenization, which can not be solved by the lexer itself, as the answer depends on the grammar. One possible solution is to follow a generate and test approach, where we generate all tokenizations, and try to parse them hoping to find a single answer, which works, but obviously is not a good idea as the number of tokenizations can be exponential. Considering this situation, we follow the tradition of lex, flex and similar lexer generators, and use a greedy approach. 

Tokenization in Pudu works as follows: first, we choose the regex that matches the longest prefix of the input, call the corresponding action (generate a token or ignore), and then continue recursively with the rest of the input. This process continues until the input ends, or no regex match a prefix. In the first case, the lexer generates the eof token by calling the eof method, and if there are no prefix matches, then the  process stops, i.e. hasNext returns false. It is advised to avoid this later case, by using a catch-all single character error regex, like in the example:
```scala
  "." { Token.ERROR(_) }
```
Another consideration is that more than one regex may match the longest prefix, for example keywords and identifiers in most programming languages. In this case, we also follow tradition, and choose the expression defined first: in the example, the string "1" would match as an IntLiteral and not as an ERROR. 

In summary, it is recommended to add the single character regex `"."`, generating an error, as the last entry of the specification.

### Language Specification
Before generating a parser, we need to specify the grammar augmented with semantic actions. In Pudu, this is done in a separate class, in order to facilitate a modular design, where the same grammar can be used for different parser generators. This design simplifies the implementation of new generators, and also allows the users to compare different parsers of the same language.

To specify a language, we need to extend `pudu.grammar.LanguageSpec[Tree, Token]`, which takes two type parameters: Tree and Token. `Tree` is a supertype of all the values generated by semantic actions, for example, the top type of an ADT, an union type, or just `Any`. On the other hand, `Token` is the same enum used by the lexer, as defined above.

A language specification contains several elements:
- Grammar symbols: There must be one term -usually a val- for each terminal and non terminal in the grammar. These values are declared using the `Terminal[+T <: reflect.Enum]` and `NonTerminal[+T]` classes. For example, in:
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
  val parserGen = SLRParserGenerator(Calc)
  val parser: Iterator[Token] => Either[ErrorMsg, Tree] = parserGen.parser
```
and finally, we can compose both into a single function, or simply nest them:
```scala
  val strParser = parser.compose(lexer)
  val result = strParser("2 * 3 - 4") // = Right(2)
```

### Reporting
Pudu also includes a class `pudu.parser.generator.SLRReport`, which generates string representations of a parser generated by SLRParserGenerator. For example:
```scala
  val parserGen = SLRParserGenerator(Calc)
  val reporter = SLRReport(parserGen)
  
  println(reporter.actionTable)
```
would print the actions table. SLRReport also contains methods to print the productions, states, LR0 automaton, FIRST and FOLLOW.

## Code Organization

There are three packages:
- pudu.grammar
- pudu.lexer
- pudu.parser

### pudu.grammar package
This package contains the code needed to transform a language specification to the internal representation used by parser generators. Main classes are:
- pudu.grammar.Symbol: ADT for grammar symbols, i.e. Terminal[Tree] and NonTerminal[Token]
- pudu.grammar.Precedence: Precedence definitions
- pudu.grammar.LanguageSpec: Language specification
- pudu.grammar.Rule: Internal representation of productions

### pudu.lexer
As the name implies, this is the lexer implementation.

### pudu.parser
Contains the parser generators. For now, that is only SLR, but we intend to add LR(1) and LALR in the future.

## Writing a Custom Parser Generator
To implement a parser generator, it is recommended to use a `LanguageSpec` object as parameter, which exposes:
- `rules: Set[Rule[Tree, Token]]`: Set of productions, where `Rule[Tree, Token]` is defined as:
```scala
  case class Rule[Tree, Token](left: NonTerminal[Tree], 
                               right: Seq[Symbol], 
                               action: Seq[Tree|Token] => (Tree|Token)):
```
- `terminals: Set[Symbol]`: Set of terminals. 
- `nonTerminals: Set[Symbol]`: Set of non terminals.
- `start: Symbol`: start symbol of the grammar.
- `eof: Terminal[Token]`: end of file terminal.
- `error: Terminal[Token]`: lexical error terminal.
- `precedence: Precedence`: operator precedence rules.

Pudu also includes the class `pudu.parser.generator.LRParserGenerator`, which contains implementations of methods common to LR parsers, such as state closure, first and follow.
