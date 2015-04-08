import java.io.PrintWriter

import mathlogic.propositionalCalculus.Deduction

/**
 * @author sugak andrey
 */
object Task2 {

  def main(args: Array[String]): Unit = {
    for (i <- 0 to 2) {
      val out = new PrintWriter(s"src/test/results/HW2/contra${if (i > 0) i else ""}.out")
      Deduction(s"src/test/resources/HW2/contra${if (i > 0) i else ""}.in").apply.foreach(out.println(_))
      out.close()
    }
  }
}
