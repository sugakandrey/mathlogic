package utils.propositionalCalculus

/**
 * @author sugak andrey
 */
object Test {
  def main(args: Array[String]) {
    val expr = new ExprParser()
    val expr1: Expr = expr.parse("(0'''''+b=0''''')").get
    println(expr1)
    val expr2: Expr = expr.parse("0'''''+0=0'''''").get
    println(expr2)
    println(expr1.substitutionSet(expr2))
    println(expr2.substitutionSet(expr1))
    println(expr1.substitute(Map("b" -> Term("0"))))
    println(expr2.substitute(Map("0" -> Term("b"))))
    println(expr2.isSubstituted(expr1))
//
  }

}
