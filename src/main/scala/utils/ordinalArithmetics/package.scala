package utils

/**
 * @author sugak andrey
 */
package object ordinalArithmetics {
  def zero: Atom = 0

  implicit def int2atom(n: Int): Atom = Atom(n)

  implicit def int2cnf(n: Int): CNF = CNF(Nil, Atom(n))

  implicit def atom2cnf(a: Atom): CNF = CNF(Nil, a)

  implicit def ord2cnf(ord: Ordinal): CNF_T = ord match {
    case W() => CNF(List((Atom(1), BigInt(1))), 0)
    case Nat(n) => CNF(Nil, n)
    case +(a, b) => ord2cnf(a) + ord2cnf(b)
    case *(a, b) => ord2cnf(a) * ord2cnf(b)
    case ^(a, b) => ord2cnf(a) ^ ord2cnf(b)
  }
}
