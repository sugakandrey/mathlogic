package utils.propositionalCalculus

import utils.propositionalCalculus.Deduction._

import scala.collection.{mutable => m}
import scala.io.Source._

/**
 * @author sugak andrey
 */
object ProvingUtils {
  val parser = new ExprParser
  val proofsDir = "proofs/"

  def readProof(filename: String): Array[Expr] = fromFile(s"$proofsDir$filename").getLines().map(parser.parse).map(_.get).toArray

  def isAxiom(expr: Expr, line: Int = 0): Option[Reason] = expr match {
    case Implication(a, Implication(_, b)) if a == b => Some(Axiom(1))
    case Implication(Implication(a, b), Implication(Implication(c, Implication(d, e)), Implication(f, g)))
      if (a, b, e) ==(c, d, g) && a == f => Some(Axiom(2))
    case Implication(a, Implication(b, Conj(c, d))) if (a, b) ==(c, d) => Some(Axiom(3))
    case Implication(Conj(a, _), c) if a == c => Some(Axiom(4))
    case Implication(Conj(_, b), c) if b == c => Some(Axiom(5))
    case Implication(a, Disj(b, c)) if a == b => Some(Axiom(6))
    case Implication(a, Disj(b, c)) if a == c => Some(Axiom(7))
    case Implication(Implication(a, b), Implication(Implication(c, d), Implication(Disj(e, f), g)))
      if (a, b, c) ==(e, g, f) && b == d => Some(Axiom(8))
    case Implication(Implication(a, b), Implication(Implication(c, Negation(d)), Negation(e)))
      if (a, b) ==(c, d) && a == e => Some(Axiom(9))
    case Implication(Negation(Negation(a)), b) if a == b => Some(Axiom(10))
    case Implication(Forall(x, e), phi) if e.isSubstituted(phi) => Some(Axiom(11))
    case Implication(Forall(x, e), phi) if (substitution(e, phi) match {
      case Some(y) => !e.isFreeForSubstitution(y, x)
      case None => false
    }) => Some(NotFreeForSubstitution(substitution(e, phi).get, x, e, line))
    case Implication(phi, Exists(x, e)) if phi.isSubstituted(e) => Some(Axiom(12))
    case Implication(phi, Exists(x, e)) if (substitution(e, phi) match {
      case Some(y) => !e.isFreeForSubstitution(y, x)
      case None => false
    }) => Some(NotFreeForSubstitution(substitution(e, phi).get, x, e, line))
    case Implication(Predicate("=", a, b), Predicate("=", Term("'", c), Term("'", d))) if (a, b) == (c, d) => Some(Axiom(13))
    case Implication(Predicate("=", a, b), Implication(Predicate("=", c, d), Predicate("=", e, f)))
      if (a, b) == (c, e) && d == f => Some(Axiom(14))
    case Implication(Predicate("=", Term("'", a), Term("'", b)), Predicate("=", c, d)) if (a, b) == (c, d) => Some(Axiom(15))
    case Negation(Predicate("=", Term("'", a), Term("0"))) => Some(Axiom(16))
    case Predicate("=", Term("+", a, Term("'", b)), Term("'", Term("+", c, d))) if (a, b) == (c, d) => Some(Axiom(16))
    case Predicate("=", Term("+", a, Term("0")), b) if a == b => Some(Axiom(17))
    case Predicate("=", Term("*", a, Term("0"), Term("0"))) => Some(Axiom(18))
    case Predicate("=", Term("*", a, Term("'", b)), Term("+", Term("*", c, d), e)) if (a, b) == (c, d) && a == e => Some(Axiom(19))
    case Implication(Conj(phi, Forall(x, Implication(psi, xi))), theta) if {
      psi == theta && psi.entersFree(x) &&
      psi.substitute(Map(x.toString -> Term("0"))) == phi &&
      psi.substitute(Map(x.toString -> Term("'", x))) == xi
    } => Some(Axiom(20))
    case _ => None
  }

  def substitution(e: Expr, d: Expr): Option[Expr] = e.substitutionSet(d) match {
    case Some(set) if set.size == 1 =>
      set.find(_ => true).filter({ case (ex, t) => e.substitute(Map(t.name -> ex)) == d}).map(_._1)
    case None => None
    case _ => None
  }

  def implicationReversed(a: Expr, b: Expr, c: Expr) = readProof("implicationReversed").map(_.substitute(Map("a" -> a, "b" -> b, "c" -> c)))

  def excludedMiddle(e: Expr) = readProof("excludedMiddle").map(_.substitute(Map("a" -> e)))

  def doubleNegation(e: Expr) = readProof("doubleNegation").map(_.substitute(Map("a" -> e)))

  def negDisjunction(a: Expr, b: Expr) = readProof("negDisjunction").map(_.substitute(Map("a" -> a, "b" -> b)))

  def implication(a: Expr, b: Expr) = readProof("implication").map(_.substitute(Map("a" -> a, "b" -> b)))

  def negImplication(a: Expr, b: Expr) = readProof("implication2").map(_.substitute(Map("a" -> a, "b" -> b)))

  def implicationToConj(a: Expr, b: Expr, c: Expr) = readProof("implicationToConj").map(_.substitute(Map("a" -> a, "b" -> b, "c" -> c)))

  def conjToImplication(a: Expr, b: Expr, c: Expr) = readProof("conjToImplication").map(_.substitute(Map("a" -> a, "b" -> b, "c" -> c)))

  def reflexivity(e: Expr): Array[Expr] = Array(axiom(1, e, e),
                                          axiom(1, e, Implication(e, e)),
                                          axiom(2, e, Implication(e, e), e),
                                          axiom(2, e, Implication(e, e), e).rhs,
                                          Implication(e, e))
}