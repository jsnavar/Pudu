package pudu.grammar


/** Classes to specify grammar productions. */

abstract class Symbol

/** Type T refers to the object to be generated by the parser, e.g. an AST node */
class NonTerminal[+T] extends Symbol

/** Tokens in pudu are defined with 'enum's, so Token must be a subtype
 *  of scala.reflect.Enum
 *  It is important to note that values associated with some terminals, like
 *  literals or identifiers, are part of the Token objects generated by the lexer,
 *  and not included in the Terminal class */
class Terminal[+Token <: scala.reflect.Enum] extends Symbol
