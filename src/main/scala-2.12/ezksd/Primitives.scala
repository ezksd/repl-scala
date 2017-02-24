package ezksd

import scala.collection.mutable

object Primitives extends {

  type Prim = List[Any] => Any

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
      "cons" -> biOp((a, b) => b match {
        case xs: List[Any] => a :: xs
        case _ => (a, b)
      }),
      "car" -> unOp { case (a, b) => a case a :: _ => a },
      "cdr" -> unOp { case (a, b) => b case _ :: xs => xs },
      "list" -> of(identity),
      "display" -> unOp { case Str(s) => print(s) }
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

  def of(op: List[Any] => Any): Primitive = op(_)

}
