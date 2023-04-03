package pudu.grammar

/** Grammar rules/productions */

case class Rule(left: Symbol, right: Seq[Symbol], reduce: Seq[Any]=>Seq[Any])
