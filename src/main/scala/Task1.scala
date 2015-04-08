import java.io.PrintWriter

import mathlogic.propositionalCalculus.Checker

/**
 * @author sugak andrey
 */
object Task1 {


  def main(args: Array[String]): Unit = {
    for (i <- 1 to 6;
         name <- List("good", "wrong")) {

      Checker(s"src/test/resources/HW1/$name$i.in").apply() match {
        case (Some(err), proof) =>
          val out = new PrintWriter(s"src/test/results/HW1/$name$i.out")
          out.println(s"Доказательство не верно начиная со строки ${err.line}\n$err")
          out.close()
        case (_, proof) =>
          val out = new PrintWriter(s"src/test/results/HW1/$name$i.out")
          out.println("Доказательсво верно.")
          proof.foreach { case (line, expr, annotation) => out.println(s"($line) $expr ($annotation)") }
          out.close()
      }
    }
  }

}
