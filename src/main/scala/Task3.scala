/**
 * @author sugak andrey
 */

import java.io.PrintWriter
import scala.io.Source.fromFile

import mathlogic.propositionalCalculus.{ExprParser, ProofMaker => PM}

object Task3 {
  val parser = ExprParser()

  def main(args: Array[String]): Unit = {
    for (i <- 1 to 7) {
      val out = new PrintWriter(s"src/test/results/HW3/true$i.out")
      val s = fromFile(s"src/test/resources/HW3/true$i.in").getLines().toArray.head
      PM.makeProof(parser.parse(s).get) match {
        case Left(p) => p._2.foreach(out.println); out.close()
        case Right(err) => println(err)
      }
    }
  }
}


