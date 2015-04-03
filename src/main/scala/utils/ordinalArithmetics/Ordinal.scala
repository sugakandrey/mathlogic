package utils.ordinalArithmetics

import scala.{BigInt => BInt}
/**
 * @author sugak andrey
 */
sealed trait Ordinal
case class *(lhs: Ordinal, rhs: Ordinal) extends Ordinal
case class +(lhs: Ordinal, rhs: Ordinal) extends Ordinal
case class ^(lhs: Ordinal, rhs: Ordinal) extends Ordinal
case class W() extends Ordinal
case class Nat(i: Int) extends Ordinal

sealed trait CNF_T extends Ordered[CNF_T] {

  override def compare(that: CNF_T): Int = (this, that) match {
    case (CNF(Nil, Atom(i)), CNF(Nil, Atom(j))) => i compare j
    case (CNF(x :: _, _), _) => 1
    case (_, CNF(x :: _, _)) => -1
    case _ if firstExp != that.firstExp => firstExp compare that.firstExp
    case _ if firstCoeff != that.firstCoeff => firstCoeff compare that.firstCoeff
    case _ => rest compare that.rest
  }

  def *(that: CNF_T): CNF = (this, that) match {
    case _ if this == zero || that == zero => zero
    case (CNF(Nil, Atom(i)), CNF(Nil, Atom(j))) => i * j: CNF
    case (_, CNF(Nil, Atom(i))) => (firstExp, firstCoeff * i) :: rest
    case _ => (firstExp + that.firstExp, that.firstCoeff) :: (this * that.rest)
  }

  def ^(that: CNF_T): CNF_T = this

  def +(that: CNF_T): CNF = (this, that) match {
    case (CNF(Nil, Atom(i)), CNF(Nil, Atom(j))) => CNF(Nil, i + j)
    case (_, that@CNF(_, _)) if firstExp < that.firstExp => that
    case (_, that@CNF(_, _)) if firstExp == that.firstExp => (firstExp, firstCoeff + that.firstCoeff) :: that.rest
    case _ => (firstExp, firstCoeff) :: (rest + that)
  }

  def -(that: CNF_T): CNF = (this, that) match {
    case (CNF(Nil, Atom(i)), CNF(Nil, Atom(j))) => if (i > j) i - j else 0
    case _ if firstExp < that.firstExp => 0
    case (c@CNF(_, _), _) if firstExp > that.firstExp => c
    case _ if firstCoeff < that.firstCoeff => 0
    case _ if firstCoeff > that.firstCoeff => (firstExp, firstCoeff - that.firstCoeff) :: rest
    case _ => rest - that.rest
  }

  def first: CNF_T = this match {
    case c@CNF(Nil, _) => c
    case CNF(x :: _, _) => CNF(x :: Nil, 0)
  }

  def size: BInt = this match {
    case CNF(Nil, _) => 1
    case CNF(xs, _) => firstExp.size + rest.size
  }

  def rest: CNF = this match {
    case CNF(x :: Nil, atom) => CNF(Nil, atom)
    case CNF(x :: xs, atom) => CNF(xs, atom)
  }

  def firstExp: CNF_T = this match {
    case CNF(x :: _, a) => CNF(Nil, x._1.asInstanceOf[Atom])
    case _ => CNF(Nil, 0)
  }

  def firstCoeff: BInt = this match {
    case CNF(Nil, Atom(i)) => i
    case CNF(x :: _, _) => x._2
  }
}

case class CNF(cnf: List[(CNF_T, BInt)], atom: Atom) extends CNF_T  {
  def ::(pair: (CNF_T, BInt)): CNF = CNF(pair :: cnf, atom)
}

case class Atom(i: Int) extends CNF_T
