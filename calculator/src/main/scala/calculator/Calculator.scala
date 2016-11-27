package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    def e(s: Signal[Expr]): Signal[Double] = Signal(eval(s(), namedExpressions))

    namedExpressions mapValues e
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def e(x: Expr) = eval(x, references)

    expr match {
      case Ref(r) => {
        if (references.contains(r)) {
          eval(references(r)(), references - r)
        } else Double.NaN
      }
      case Literal(d) => d
      case Plus(x, y) => e(x) + e(y)
      case Times(x, y) => e(x) * e(y)
      case Minus(x, y) => e(x) - e(y)
      case Divide(x, y) => e(x) / e(y)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
