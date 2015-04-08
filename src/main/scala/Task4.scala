import java.io.PrintWriter

import mathlogic.propositionalCalculus.{Checker, Deduction}

/**
 * @author sugak andrey
 */
object Task4 {
  def main(args: Array[String]) {
    for (i <- 1 to 14; name <- Seq("correct", "incorrect")) {
      println(s"Testing $i")
      val out = new PrintWriter(s"src/test/results/HW4/$name$i.out")
      val s = s"src/test/resources/HW4/$name$i.in"
      Checker(s).apply() match {
        case (Some(err), proof) =>
          out.println(s"Доказательство не верно начиная со строки ${err.line}")
          proof.foreach { case (line, expr, annotation) => out.println(s"($line) $expr ($annotation)") }
        case (_, proof) => Deduction(s).apply.foreach(out.println)
      }
      out.close()
    }
  }
}
