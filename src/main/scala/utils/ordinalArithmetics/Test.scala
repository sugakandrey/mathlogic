package utils.ordinalArithmetics

/**
 * @author sugak andrey
 */
object Test {

  def main(args: Array[String]): Unit = {
    val a = CNF(Nil, 100)
    val b = CNF(Nil, 200)
    val c = CNF((Atom(1), BigInt(1)) :: Nil, 10)
    val d = CNF((Atom(1), BigInt(2)) :: Nil, 0)
    println(c.firstExp)
    println(d.firstExp)
    println(c + d)
  }

}
