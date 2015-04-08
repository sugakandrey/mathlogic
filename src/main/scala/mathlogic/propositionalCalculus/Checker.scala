package mathlogic.propositionalCalculus

import mathlogic.propositionalCalculus.Checker.MultiMap
import mathlogic.propositionalCalculus.ProvingUtils._

import scala.collection.{mutable => m}
import scala.io.Source.fromFile

/**
 * @author sugak andrey
 */

class Checker( proof: Array[Expr],
               alpha: Option[Expr],
               assumptions: Option[m.HashMap[Expr, (Expr, Int)]],
               implications: MultiMap[Expr, (Implication, Int)],
               stored: m.HashMap[Expr, (Expr, Int)],
               var failure: Option[Error]) {

  type Annotation = (Int, Expr, Reason)

  def apply(): (Option[Error], Array[Annotation]) = {
    val result = proof.zipWithIndex.map(process)
    failure match {
      case Some(f) => (Some(f), result)
      case _ => (None, result)
    }
  }

  def saveEntries(entry: (Expr, Int)): Unit = {
    stored += entry._1 -> entry
    entry._1 match {
      case i@Implication(_, second) => implications.addBinding(second, (i, entry._2))
      case _ =>
    }
  }

  def MP(e: Expr, line: Int): Option[Reason] = {
    implications.get(e) match {
      case Some(set) => set.find { case (impl, i) => stored.contains(impl.lhs) } match {
        case Some((x, j)) => Some(ModusPonens(stored.get(x.lhs).get._2, j))
        case _ => None
      }
      case _ => None
    }
  }

  def setErr(err: Error): Reason = {
    failure = failure.orElse(Some(err))
    err
  }

  def assumed(e: Expr): Boolean = assumptions.exists(_.contains(e))

  def assumption(e: Expr): Assumption = Assumption(assumptions.flatMap(_.get(e)).get._2)

  def inferenceRules(e: Expr, line: Int): Reason = e match {
    case expr if MP(e, line).isDefined => MP(e, line).get
    case Implication(Exists(x, psi), phi)
      if !phi.entersFree(x) && stored.contains(Implication(psi, phi)) &&
        assumptions.exists(_.values.map(_._1).exists(_.entersFree(x))) =>
                            val expr = assumptions.map(_.values).get.find{case (a: Expr, _) => a.entersFree(x)}.get._1
                            setErr(InferenceRuleOnFreeVar(x, expr, line))
    case Implication(phi, Forall(x, psi))
      if !phi.entersFree(x) && stored.contains(Implication(phi, psi)) &&
      assumptions.exists(_.values.map(_._1).exists(_.entersFree(x))) =>
                            val expr = assumptions.map(_.values).get.find{case (a: Expr, _) => a.entersFree(x)}.get._1
                            setErr(InferenceRuleOnFreeVar(x, expr, line))
    case Implication(Exists(x, psi), phi)
      if !phi.entersFree(x) && stored.contains(Implication(psi, phi)) => InferenceExists(stored.get(Implication(psi, phi)).get._2)
    case Implication(Exists(x, psi), phi)
      if phi.entersFree(x) && stored.contains(Implication(psi, phi)) => setErr(EntersFreely(x, phi, line))
    case Implication(phi, Forall(x, psi))
      if !phi.entersFree(x) && stored.contains(Implication(phi, psi)) => InferenceForall(stored.get(Implication(phi, psi)).get._2)
    case Implication(phi, Forall(x, psi))
      if phi.entersFree(x) && stored.contains(Implication(phi, psi)) => setErr(EntersFreely(x, phi, line))
    case _ => setErr(Mistake(line, "Выражение не может быть выведено"))
  }

  def annotate(e: Expr, line: Int): Reason = isAxiom(e, line) match {
    case Some(err: Error) => setErr(err)
    case Some(axiom: Axiom) => axiom
    case _ => assumed(e) match {
      case true => assumption(e)
      case false => inferenceRules(e, line)
    }
  }

  def process(p: (Expr, Int)): (Int, Expr, Reason) = {
    val (e, line) = p
    val res = (line + 1, e, annotate(e, line + 1))
    saveEntries(e, line + 1)
    res
  }
}


object Checker {
  type MultiMap[T, E] = m.HashMap[T, m.Set[E]] with m.MultiMap[T, E]

  def apply(filename: String) = {
    val lines = fromFile(filename).getLines().filter(!_.isEmpty).toArray
    val parser = ExprParser()
    val stored = m.HashMap[Expr, (Expr, Int)]()
    val ctx: Option[Ctx] = parser.parseCtx(lines(0))
    val proof = lines.drop(if (ctx.isDefined) 1 else 0).map(parser.parse(_).get)
    new Checker(proof, ctx.map(_.alpha), ctx.map(_.assumptions),
                new m.HashMap[Expr, m.Set[(Implication, Int)]]() with m.MultiMap[Expr, (Implication, Int)],
                stored, None)
  }
}
