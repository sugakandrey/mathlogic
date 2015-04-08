package mathlogic.propositionalCalculus

/**
 * @author sugak andrey
 */
import org.parboiled2._

import scala.collection.{mutable => m}
import scala.util.{Failure, Success}


case class ExprParser() {

  def parse(s: String): Option[Expr] = {
    new ParboiledParser(s).inputLine.run() match {
      case Success(expr) => Some(expr)
      case Failure(e: ParseError) => None
    }
  }
  def parseT(s: String): Option[Term] = {
    new ParboiledParser(s).term.run() match {
      case Success(expr) => Some(expr)
      case Failure(e: ParseError) => None
    }
  }

  def parseP(s: String): Option[Expr] = {
    new ParboiledParser(s).predicate.run() match {
      case Success(expr) => Some(expr)
      case Failure(e: ParseError) => None
    }
  }


  def parseCtx(s: String): Option[Ctx] = {
    new ParboiledParser(s).ctx.run() match {
      case Success(es) => Some(es)
      case _ => None
    }
  }

  def parseCtxD(s: String): Option[Ctx] = {
    new ParboiledParser(s).ctxD.run() match {
      case Success(es) => Some(es)
      case _ => None
    }
  }

  class ParboiledParser(val input: ParserInput) extends Parser {

    implicit def wspStr(s: String): Rule0 = rule {
      zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ')
    }

    def intoMap(xs: List[Expr]): m.HashMap[Expr, Expr] = {
      var assumptions = m.HashMap[Expr, Expr]()
      xs.take(xs.size - 1).foreach(x => assumptions += ((x, x)))
      assumptions
    }

    private def leftAssoc[A](a: => Rule1[A], b: (A, A) => A, divider: String): Rule1[A]
    = rule { a ~ zeroOrMore(wspStr(divider) ~ a ~> b) }

    def ctx: Rule1[Ctx] =
      rule { (zeroOrMore(expr).separatedBy(",")
        ~> ((xs: Seq[Expr]) => Ctx(xs.toList, xs.last)) ~ "|-" ~ expr ~> ((ctx: Ctx, _: Expr) => ctx) ~ EOI)}

     def ctxD: Rule1[Ctx] =
      rule { (oneOrMore(expr).separatedBy(",")
        ~> ((xs: Seq[Expr]) => Ctx(xs.toList.init, xs.last)) ~ "|-" ~ expr ~> ((ctx: Ctx, _: Expr) => ctx) ~ EOI)}


    def inputLine: Rule1[Expr] = rule { expr ~ EOI }
    private def expr: Rule1[Expr] = rule { disjunction ~ zeroOrMore("->" ~ expr ~> Implication) }
    private def disjunction: Rule1[Expr] = rule { conjunction ~ zeroOrMore("|" ~ conjunction ~> Disj) }
    private def conjunction: Rule1[Expr] = rule { unary ~ zeroOrMore("&" ~ unary ~> Conj) }

    private def unary: Rule1[Expr] = rule { predicate | negation | parens | ("@" ~ variable ~ unary ~> ((a, b) => Forall(a, b))) |
      ("?" ~ variable ~ unary ~> ((a, b) => Exists(a, b))) }
    def predicate: Rule1[Expr] = rule { term ~ "=" ~ term ~> ((a: Term, b: Term) => Predicate("=", a, b)) |
      capture(oneOrMore(CharPredicate.UpperAlpha) ~ zeroOrMore(CharPredicate.AlphaNum)) ~ optional("(" ~ zeroOrMore(term).separatedBy(",") ~ ")") ~>
        ((a: String, b:Option[Seq[Term]]) => if (b.isEmpty) Term(a) else Predicate(a, b.get: _*)) | term }
    def term: Rule1[Term] =
      leftAssoc(summable, (a: Term, b: Term) => Term("+", a, b), "+")
    private def summable: Rule1[Term] =
      leftAssoc(mullable, (a: Term, b: Term) => Term("*", a, b), "*")
    private def mullable: Rule1[Term] =
      rule {
        ((capture(CharPredicate.LowerAlpha) ~
          "(" ~
          oneOrMore(term).separatedBy(",") ~
          ")" ~>
          ((a: String, b: Seq[Term]) => Term(a, b: _*))) |
          variable |
          ("(" ~ term ~ ")") |
          (str("0") ~> (() => Term("0")))) ~
          zeroOrMore(capture("'")) ~> ((a: Term, b: Seq[_]) => wrapInQuote(a, b.length)) }

    private def wrapInQuote(e: Term, n: Int): Term = {
      if (n < 1) e else wrapInQuote(Term("'", e), n - 1)
    }

    private def negation: Rule1[Expr] = rule { "!" ~ unary ~> Negation }
    private def variable: Rule1[Term] = rule { capture(letters) ~> ((a: String) => Term(a)) }
    private def letters: Rule0 = rule { oneOrMore(CharPredicate.LowerAlpha) ~ zeroOrMore(CharPredicate.Digit) }
    private def parens: Rule1[Expr] = rule { "(" ~ expr ~ ")" }
  }

}