package utils.propositionalCalculus

import scala.collection.{mutable => m}

/**
 * @author sugak andrey
 */
sealed trait Expr {
  def getVars: List[Term] = this match {
    case a@Term(name) => List(a)
    case Term(name, b@_*) => b.foldLeft(List[Term]())((l, t) => t.getVars ::: l)
    case Predicate(name, b@_*) => b.foldLeft(List[Term]())((l, t) => t.getVars ::: l)
    case Conj(a, b) => a.getVars ++ b.getVars
    case Implication(a, b) => a.getVars ++ b.getVars
    case Disj(a, b) => a.getVars ++ b.getVars
    case Negation(a) => a.getVars
  }

  def substitute(implicit variables: Map[String, Expr]): Expr = this match {
    case Forall(x, b) => Forall(x, b.substitute(variables))
    case Exists(x, b) => Exists(x, b.substitute(variables))
    case Term(name) if variables.get(name).isDefined => variables.get(name).get
    case v@Term(name) => v
    case Conj(a, b) => Conj(a.substitute, b.substitute)
    case Implication(a, b) => Implication(a.substitute, b.substitute)
    case Disj(a, b) => Disj(a.substitute, b.substitute)
    case Negation(a) => Negation(a.substitute)
    case Predicate(name, b@_*) => Predicate(name, b.map(_.substitute(variables)).map({case t: Term => t}):_*)
    case Term(name, b@_*) => Term(name, b.map(_.substitute(variables)).map({case t: Term => t}):_*)
  }

  def entersFree(e: Term, affectedVars: Set[Term] = Set()): Boolean = {
    this match {
      case Forall(x, b) => b.entersFree(e, affectedVars + x)
      case Exists(x, b) => b.entersFree(e, affectedVars + x)
      case Implication(a, b) => a.entersFree(e, affectedVars) || b.entersFree(e, affectedVars)
      case Disj(a, b) => a.entersFree(e, affectedVars) || b.entersFree(e, affectedVars)
      case Conj(a, b) => a.entersFree(e, affectedVars) || b.entersFree(e, affectedVars)
      case Negation(a) => a.entersFree(e, affectedVars)
      case t@Term(_) if t == e => !affectedVars.contains(t)
      case Term(name, b@_*) => b.exists(_.entersFree(e, affectedVars))
      case Predicate(name, b@_*) => b.exists(_.entersFree(e, affectedVars))
      case _ => false
    }
  }

  def isFreeForSubstitution(e: Expr, x: Term, affectedVars: Set[Term] = Set()): Boolean = this match {
    case Forall(y, phi) => phi.isFreeForSubstitution(e, x, affectedVars + y)
    case Exists(y, phi) => phi.isFreeForSubstitution(e, x, affectedVars + y)
    case Implication(a, b) => a.isFreeForSubstitution(e, x, affectedVars) && b.isFreeForSubstitution(e, x, affectedVars)
    case Disj(a, b) => a.isFreeForSubstitution(e, x, affectedVars) && b.isFreeForSubstitution(e, x, affectedVars)
    case Conj(a, b) => a.isFreeForSubstitution(e, x, affectedVars) && b.isFreeForSubstitution(e, x, affectedVars)
    case t@Term(name) if affectedVars.contains(t) && name == x.toString => false
    case Term(name) if name == x.toString => e.getVars.forall(!affectedVars.contains(_))
    case Term(name, b@_*) => b.forall(_.isFreeForSubstitution(e, x, affectedVars))
    case Predicate(name, b@_*) => b.forall(_.isFreeForSubstitution(e, x, affectedVars))
    case _ => true
  }


  def merge(mapA: Option[Set[Sub]],
            mapB: Option[Set[Sub]]): Option[Set[Sub]] = Some(mapA.getOrElse(Set()) ++ mapB.getOrElse(Set()))

  def isSubstituted(e: Expr): Boolean = substitutionSet(e) match {
    case Some(set) if set.size <= 1 =>
      set.forall(p => isFreeForSubstitution(p._1, p._2) && substitute(Map(p._2.name -> p._1)) == e)
    case _ => false
  }

  type Sub = (Expr, Term)
  def substitutionSet(e: Expr): Option[Set[Sub]] = this match {
    case Implication(a, b) => e match {
      case Implication(c, d) => merge(a.substitutionSet(c), b.substitutionSet(d))
      case _ => None
    }
    case Disj(a, b) => e match {
      case Disj(c, d) => merge(a.substitutionSet(c), b.substitutionSet(d))
      case _ => None
    }
    case Conj(a, b) => e match {
      case Conj(c, d) => merge(a.substitutionSet(c), b.substitutionSet(d))
      case _ => None
    }
    case Negation(a) => e match {
      case Negation(b) => a.substitutionSet(b)
      case _ => None
    }
    case Forall(t, ex) => e match {
      case Forall(g, dx) => ex.substitutionSet(dx)
      case _ => None
    }
    case Exists(t, ex) => e match {
      case Exists(g, dx) => ex.substitutionSet(dx)
      case _ => None
    }
    case Predicate(name, b@_*) => e match {
      case Predicate(name2, b2@_*) if name == name2 && b.length == b2.length =>
        if (b.length == 0) Some(Set())
        else b.zip(b2).map(p => p._1.substitutionSet(p._2)).foldRight(Option.empty[Set[Sub]])(merge)
      case _ => None
    }
    case t@Term(name) if t != e => Some(Set(e -> t))
    case Term(name, b@_*) => e match {
      case Term(name2, b2@_*) if name == name2 && b.length == b2.length =>
        if (b.length == 0) Some(Set())
        else b.zip(b2).map(p => p._1.substitutionSet(p._2)).foldRight(Option.empty[Set[Sub]])(merge)
      case _ => None
    }
    case _ => None
  }
}

sealed trait Quantifier extends Expr

case class Forall(x: Term, e: Expr) extends Quantifier { override def toString = s"@$x($e)" }

case class Exists(x: Term, e: Expr) extends Quantifier { override def toString = s"?$x($e)" }

case class Implication(lhs: Expr, rhs: Expr) extends Expr { override def toString = s"($lhs->$rhs)" }

case class Negation(body: Expr) extends Expr { override def toString = s"!($body)" }

case class Conj(lhs: Expr, rhs: Expr) extends Expr { override def toString = s"($lhs&$rhs)" }

case class Disj(lhs: Expr, rhs: Expr) extends Expr { override def toString = s"($lhs|$rhs)" }

case class Predicate(name: String, body: Term*) extends Expr {
  val commonPredicates = Seq("=", "*", "+")
  override def toString = if (body.length == 2 && commonPredicates.contains(name))
                                    s"${body(0)} $name ${body(1)}"
                                  else if (body.length == 0) name
                                  else s"$name(${body.mkString(",")})"
}

case class Term(name: String, body: Term*) extends Expr {
  val commonPredicates = Seq("=", "*", "+")
  override def toString = if (body.length == 2 && commonPredicates.contains(name))
                            s"${body(0)} $name ${body(1)}"
                          else if (name == "'") s"(${body(0)})'"
                          else if (body.length == 0) name
                          else s"$name(${body.mkString(",")})"
}

case class Ctx(assumptions: m.HashMap[Expr, (Expr, Int)], alpha: Expr) {
  def get(e: Expr): Option[Expr] = assumptions.get(e).map(_._1)

  def contains(e: Expr): Boolean = assumptions.contains(e)
}

object Ctx {
  def apply(xs: List[Expr], alpha: Expr) = {
    var assumptions = m.HashMap[Expr, (Expr, Int)]()
    xs.zipWithIndex.foreach { case (x, i) => assumptions += (x -> (x, i + 1)) }
    new Ctx(assumptions, alpha)
  }

  def apply(vars: Map[String, Boolean]) = {
    var assumptions = m.HashMap[Expr, (Expr, Int)]()
    val mapped = vars.map({
      case (name, true) => Term(name)
      case (name, false) => Negation(Term(name))
    })
    mapped.zipWithIndex.foreach{ case (x, i) => assumptions += (x -> (x, i + 1)) }
    new Ctx(assumptions, mapped.last)
  }
}