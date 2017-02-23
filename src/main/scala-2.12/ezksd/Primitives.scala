package ezksd

import scala.collection.mutable

object Primitives extends {

  def init: mutable.Map[String, Any] = {
    mutable.Map[String, Any](
      "+" -> numOp(_ + _),
      "-" -> numOp(_ - _),
      "*" -> numOp(_ * _),
      "/" -> numOp(_ / _),
      "%" -> numOp(_ % _),
      ">" -> numOp(_ > _),
      "<" -> numOp(_ < _),
      "=" -> numOp((a, b) => a - b < 0.00000001D),
      "cons" -> biOp((a, b) => (a, b)),
      "car" -> unOp { case (a, b) => a },
      "cdr" -> unOp { case (a, b) => b },
      "display" -> unOp {case Str(s) => print(s)}
    )
  }

  def numOp(op: (Double, Double) => Any): Primitive = {
    case List(a: Double, b: Double) => op(a, b) //idea highlight error
    case _ => throw new SyntaxException("illegal operand type")
  }

  def biOp(op: (Any, Any) => Any): Primitive = {
    case a :: b :: Nil => op(a, b)
    case _ => throw new SyntaxException("illegal operand type")
  }

  def unOp(op: Any => Any): Primitive = {
    case a :: Nil => op(a)
    case _ => throw new SyntaxException("illegal operand type")
  }



}
