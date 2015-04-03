package utils.propositionalCalculus

import utils.propositionalCalculus.Deduction._
import utils.propositionalCalculus.ProvingUtils._

import scala.collection.{mutable => m}

/**
 * @author sugak andrey
 */
class Deduction (
                 implications: m.HashMap[Expr, Implication],
                 ctx: Ctx,
                 alpha: Expr,
                 proof: Array[Expr]) {


  def apply(): Array[Expr] = (Array[Expr]() /: proof)((acc, expr) => constructProof(acc)(expr))

  def constructProof(acc: Array[Expr])(beta: Expr): Array[Expr] = {
    val wanted = Implication(alpha, beta)
    beta match {
      case i@Implication(a, b) => implications += (b -> i)
      case _ =>
    }
    if (isAxiom(beta).isDefined || ctx.contains(beta)) {
      acc :+ axiom(1, beta, alpha) :+
             beta :+
             wanted
    }
    else if (beta == alpha) {
      acc :+ axiom(1, alpha, alpha) :+
             axiom(1, alpha, Implication(alpha, alpha)) :+
             axiom(2, alpha, Implication(alpha, alpha), alpha) :+
             axiom(2, alpha, Implication(alpha, alpha), alpha).rhs :+
             wanted
    } else {
      beta match {
        case Implication(phi, q@Forall(x, psi)) => acc ++ implicationToConj(alpha, phi, psi) ++
                                                          Array(Implication(Conj(alpha, phi), psi),
                                                          Implication(Conj(alpha, phi), q)) ++
                                                          conjToImplication(alpha, phi, q) :+
                                                          wanted
        case Implication(q@Exists(x, psi), phi) => acc ++ implicationReversed(alpha, psi, phi) ++
                                                          Array(Implication(psi, Implication(alpha, phi)),
                                                          Implication(q, Implication(alpha, phi))) ++
                                                          implicationReversed(q, alpha, phi) :+
                                                          wanted
        case _ => val e = implications.get(beta).get.lhs
                  acc :+ axiom(2, alpha, e, beta) :+
                         axiom(2, alpha, e, beta).rhs :+
                         wanted
      }
    }
  }
}

object Deduction {
  def apply(fileName: String) = {
    val parser = new ExprParser()
    val input = scala.io.Source.fromFile(fileName)
    val ctx = parser.parseCtxD(input.getLines().next()).get
    val alpha = ctx.alpha
    val proof = input.getLines().map(parser.parse(_).get).toArray
    new Deduction(m.HashMap[Expr, Implication](), ctx, alpha, proof)
  }

  def axiom(num: Int, xs: Expr*): Implication = num match {
    case 1 => Implication(xs(0), Implication(xs(1), xs(0)))
    case 2 => Implication(Implication(xs(0), xs(1)), Implication(Implication(xs(0), Implication(xs(1), xs(2))), Implication(xs(0), xs(2))))
    case 3 => Implication(xs(0), Implication(xs(1), Conj(xs(0), xs(1))))
    case 4 => Implication(Conj(xs(0), xs(1)), xs(0))
    case 5 => Implication(Conj(xs(0), xs(1)), xs(1))
    case 6 => Implication(xs(0), Disj(xs(0), xs(1)))
    case 7 => Implication(xs(1), Disj(xs(0), xs(1)))
    case 8 => Implication(Implication(xs(0), xs(1)), Implication(Implication(xs(2), xs(1)), Implication(Disj(xs(0), xs(2)), xs(1))))
    case 9 => Implication(Implication(xs(0), xs(1)), Implication(Implication(xs(0), Negation(xs(1))), Negation(xs(0))))
    case 10 => Implication(Negation(Negation(xs(0))), xs(0))
  }

  def apply(ctx: Ctx, proof: Array[Expr]) = new Deduction(m.HashMap[Expr, Implication](), ctx, ctx.alpha, proof)
}
