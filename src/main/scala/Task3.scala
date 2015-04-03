/**
* @author sugak andrey
*/

import utils.propositionalCalculus.{ExprParser, ProofMaker => PM}

object Task3 {
  val parser = new ExprParser
  def main(args: Array[String]): Unit = {
    PM.makeProof(parser.parse("A->C").get) match {
      case Left(p) => p._2.foreach(println)
      case Right(err) => println(err)
    }
  }
}

