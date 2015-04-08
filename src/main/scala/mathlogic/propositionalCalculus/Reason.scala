package mathlogic.propositionalCalculus

/**
 * @author sugak andrey
 */
sealed trait Reason

case class Axiom(num: Int) extends Reason { override def toString = s"Сх. акс. $num" }
case class ModusPonens(first: Int, second: Int) extends Reason { override def toString = s"M.P. ($first, $second)" }
case class InferenceForall(line: Int) extends Reason {override def toString = s"Правило вывода для квантора всеобщности $line" }
case class InferenceExists(line: Int) extends Reason {override def toString = s"Правило вывода для квантора существования $line" }
case class Assumption(i: Int) extends Reason { override def toString = s"Предположение номер $i" }

sealed trait Error extends Reason { val line = 0 }
case class Mistake(override val line: Int, description: String*) extends Error { override def toString = s"В строке $line ${description.mkString(" ")}"}
case class NotFreeForSubstitution(t: Expr, x: Term, e: Expr, override val line: Int) extends Error {
  override def toString = s"В строке $line терм $t не свободен для подстановки вместо терма $x формулу $e"
}
case class EntersFreely(x: Term, e: Expr, override val line: Int) extends Error {
  override def toString = s"В строке $line переменная $x входит свободно в формулу $e"
}
case class InferenceRuleOnFreeVar(t: Term, e: Expr, override val line: Int) extends Error {
  override def toString = s"В строке $line используется правило вывода по переменной $t входящей свободно в предположение $e"
}

case class ProveError(vars: Map[String, Boolean]) {
  override def toString = {
    val result = "Высказывание не верно при: "
    val values = vars.keys.map(key => key + (if (vars.get(key).get) "=И" else "=Л")).mkString(", ")
    result + values
  }
}