import utils.propositionalCalculus.Deduction

/**
 * @author sugak andrey
 */
object Task2 {
  def main(args: Array[String]): Unit = {
    Deduction("task2.in").apply().foreach(println)
  }
}
