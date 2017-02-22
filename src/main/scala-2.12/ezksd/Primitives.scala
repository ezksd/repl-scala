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
      ">" -> numOp(_ > _),
      "=" -> numOp((a, b) => a - b < 0.00000001D))
  }

  def numOp(op: (Double, Double) => Any): Primitive = {
    case List(a: Double, b: Double) => op(a, b) //idea highlight error
    case _ => throw new SyntaxException("illegal operand type")
  }

}
