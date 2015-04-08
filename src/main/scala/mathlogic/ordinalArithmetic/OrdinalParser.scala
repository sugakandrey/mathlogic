package mathlogic.ordinalArithmetic

import org.parboiled2._
import mathlogic.ordinalArithmetic
import scala.util.{Failure, Success}

/**
 * @author sugak andrey
 */
case class OrdinalParser() {

  def parse(s: String): Option[(Ordinal, Ordinal)] = {
    new ParboiledParser(s).equals.run() match {
      case Success(ord) if ord.length == 2 => Some((ord.head, ord(1)))
      case Failure(_) => None
    }
  }

  class ParboiledParser(val input: ParserInput) extends Parser {
    type Ord = Rule1[Ordinal]
    implicit private def wrpStr(s: String): Rule0 = rule {
      zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ')
    }

    def equals: Rule1[Seq[Ordinal]] = rule { oneOrMore(ordinal).separatedBy("=") }
    private def ordinal: Ord = rule { sum ~ zeroOrMore("+" ~ sum ~> ordinalArithmetic.+) }
    private def sum: Ord = rule { mul ~ zeroOrMore("*" ~ mul ~> ordinalArithmetic.*) }
    private def mul: Ord = rule { parens ~ zeroOrMore("^" ~ parens ~> ordinalArithmetic.^) }
    private def parens: Ord = rule { omega | nat | "(" ~ ordinal ~ ")" }
    private def omega: Ord = rule { ch('w') ~> (() => W()) }
    private def nat: Ord = rule { capture(oneOrMore(CharPredicate.Digit)) ~> ((s: String) => Nat(Integer.parseInt(s)))}

  }
}
