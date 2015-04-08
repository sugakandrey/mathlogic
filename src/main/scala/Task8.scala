import mathlogic.ordinalArithmetic._

import scala.io.Source

/**
 * @author sugak andrey
 */
object Task8 {
  val parser = OrdinalParser()

  def main(args: Array[String]) {
    val lines = if (args.length >= 1) Seq(args(0))
                else Source.fromFile("src/test/resources/HW8/task8.in").getLines()
    lines
      .map(parser.parse(_).get)
      .map { case (x, y) => println(s"$x = $y"); (x: CNF_T, y: CNF_T) }
      .foreach { case (x, y) => if (x == y) println(s"Равны. lhs = $x rhs = $y")
    else println(s"Не равны. lhs = $x, rhs = $y")
    }
  }
}
