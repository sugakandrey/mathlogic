package utils.propositionalCalculus

/**
 * @author sugak andrey
 */

import utils.propositionalCalculus.Deduction._
import utils.propositionalCalculus.{ProvingUtils => Proofs}

object ProofMaker {
  val parser = new ExprParser()
  type Vars = Map[String, Boolean]
  type Proof = (Map[String, Boolean], Array[Expr])


  def provePrimitive(ctx: Vars, e: Expr): Array[Expr] = {
    implicit val vars = ctx
    e match {
      case Term(name) if ctx.get(name).get => Array(e)
      case Negation(Term(name)) if !ctx.get(name).get => Array(e)
      case Negation(Negation(a)) if evalInCtx(a) => provePrimitive(ctx, a) ++ Proofs.doubleNegation(a) :+ e
      case Disj(a, b) if evalInCtx(a) => provePrimitive(ctx, a) :+ axiom(6, a, b) :+ e
      case Disj(a, b) if evalInCtx(b) => provePrimitive(ctx, b) :+ axiom(7, a, b) :+ e
      case Negation(Disj(a, b)) if !evalInCtx(a) && !evalInCtx(b) => Proofs.negDisjunction(a, b)
      case Conj(a, b) if evalInCtx(a) && evalInCtx(b) =>
        provePrimitive(ctx, a) ++ provePrimitive(ctx, b) :+ axiom(3, a, b) :+ axiom(3, a, b).rhs :+ e
      case w@Negation(e@Conj(a, b)) if !evalInCtx(a) =>
        provePrimitive(ctx, Negation(a)) :+ axiom(1, Negation(a), e) :+ Implication(e, Negation(a)) :+
          axiom(4, a, b) :+ axiom(9, e, a) :+ axiom(9, e, a).rhs :+ w
      case w@Negation(e@Conj(a, b)) if !evalInCtx(b) =>
        provePrimitive(ctx, Negation(b)) :+ axiom(1, Negation(b), e) :+ Implication(e, Negation(b)) :+
          axiom(4, b, a) :+ axiom(9, e, b) :+ axiom(9, e, b).rhs :+ w
      case Implication(a, b) if evalInCtx(b) => provePrimitive(ctx, b) :+ axiom(1, b, a) :+ e
      case Implication(a, b) if !evalInCtx(a) =>
        provePrimitive(ctx, Negation(a)) ++ Proofs.implication(a, b)
      case Negation(Implication(a, b)) if evalInCtx(a) && !evalInCtx(b) => provePrimitive(ctx, a) ++
        provePrimitive(ctx, Negation(b)) ++ Proofs.negImplication(a, b)
    }
  }

  def deduce(ctx: Ctx, proof: Array[Expr]): Array[Expr] = Deduction(ctx, proof).apply()

  def evalInCtx(e: Expr)(implicit ctx: Map[String, Boolean]): Boolean = e match {
    case Term(name) => ctx.get(name).get
    case Negation(a) => !evalInCtx(a)
    case Conj(a, b) => evalInCtx(a) && evalInCtx(b)
    case Disj(a, b) => evalInCtx(a) || evalInCtx(b)
    case Implication(a, b) => !evalInCtx(a) || evalInCtx(b)
  }

  def makeProof(e: Expr): Either[Proof, ProveError] = buildProof(e, e.getVars.map(_.toString).distinct)

  def merge(tuple1: Proof, tuple2: Proof, e: Expr): Proof  = {
    val (vars1, proof1) = tuple1
    val (vars2, proof2) = tuple2
    val excluded = Term(vars1.last._1)
    val deducedProof1 = deduce(Ctx(vars1), proof1)
    val deducedProof2 = deduce(Ctx(vars2), proof2)
    val axiom8 = axiom(8, excluded, e, Negation(excluded))
    val mergedProof = deducedProof1 ++
                      deducedProof2 ++
                      Proofs.excludedMiddle(excluded) :+
                      axiom8 :+
                      axiom8.rhs :+
                      Implication(Disj(excluded, Negation(excluded)), e) :+
                      e
    (vars1.take(vars1.size - 1), mergedProof)
  }

  def buildProof(e: Expr, vars: List[String], assumptions: Vars = Map()): Either[Proof, ProveError] = {
    implicit val ctx = assumptions
    vars match {
      case x :: xs => val proof1 = buildProof(e, xs, assumptions + (x -> true))
                      val proof2 = buildProof(e, xs, assumptions + (x -> false))
                      val proofOrError1 = proof1.left.getOrElse(Map[String, Boolean](), Array[Expr]())
                      val proofOrError2 = proof2.left.getOrElse(Map[String, Boolean](), Array[Expr]())
                      (proofOrError1._2, proofOrError2._2) match {
                        case (Array(), _) => proof1
                        case (_, Array()) => proof2
                        case _ => Left(merge(proofOrError1, proofOrError2, e))
                      }
      case Nil if evalInCtx(e) => val proof = provePrimitive(assumptions, e)
                                  Left(assumptions, proof)
      case _ => Right(ProveError(assumptions))
    }
  }
}