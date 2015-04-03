import utils.propositionalCalculus.Checker

/**
 * @author sugak andrey
 */
object Task1 {
  def main(args: Array[String]): Unit = {
    Checker("result").apply() match {
      case (Some(err), proof) =>
        println(s"Доказательство не верно начиная со строки ${err.line}\n$err")
        proof.foreach{ case (line, expr, annotation) => println(s"($line) $expr ($annotation)") }
      case (_, proof) =>
        println("Доказательсво верно.");
        proof.foreach{ case (line, expr, annotation) => println(s"($line) $expr ($annotation)") }
    }
  }

}
