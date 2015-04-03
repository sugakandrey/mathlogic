import java.io.PrintWriter

import utils.propositionalCalculus.{Checker, Deduction}

/**
 * @author sugak andrey
 */
object Task4 {
  def main(args: Array[String]) {
    val out = new PrintWriter("result")
    val path: String = "/Users/2th3sk13s/Desktop/logic2014/tests/HW4/incorrect11.in"
    Checker(path).apply() match {
      case (Some(err), proof) =>
        println(s"Доказательство не верно начиная со строки ${err.line}")
        proof.foreach{ case (line, expr, annotation) => println(s"($line) $expr ($annotation)") }
      case (_, proof) =>
        println("Доказательсво верно."); proof.foreach{ case (line, expr, annotation) => println(s"($line) $expr ($annotation)")}
        Deduction(path).apply().foreach(out.println)
        out.close()
        Checker("result").apply() match {
          case (Some(err), proof1) => println("Test failed")
          case _ => println("Test passed")
        }
    }
  }
}
