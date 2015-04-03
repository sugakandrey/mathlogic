package utils.ordinalArithmetics

import org.parboiled2._

/**
 * @author sugak andrey
 */
abstract class OrdParser extends Parser {
//  type RO = Rule1[Ordinal]
//  implicit private def wrpStr(s: String): Rule0 = rule {
//    zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ')
//  }
//
//  private def leftAssoc[A](a: => Rule1[A], b: (A, A) => A, divider: String): Rule1[A]
//  = rule { a ~ zeroOrMore(wrpStr(divider) ~ a ~> b) }
//
//  def equalityOrdinals: Rule1[(Ordinal, Ordinal)] =
//    rule {
//      oneOrMore(ordExpr).separatedBy("=") ~> ((sq: Seq[Ordinal]) => (sq(0), sq(1)))
//    }
//
//  def ordExpr: RO  = leftAssoc(summable, ordinals.+, "+")
//  private def summable: RO = unary
//  private def unary: RO = rule { omega | numeric | ("(" ~ ordExpr ~ ")") }
//  private def omega: RO = rule { ch('w') ~> (() => W()) }
//  private def numeric: RO =
//    rule { capture(oneOrMore(anyOf("0123456789"))) ~>
//      ((s: String) => Num(Integer.parseInt(s)))
//    }
}
