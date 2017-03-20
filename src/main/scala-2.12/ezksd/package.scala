import scala.util.control.TailCalls.TailRec

package object ezksd {

//  abstract class Primitive() extends Function[List[Any], Any]

//  abstract class Primitive extends Function[List[Any],Any]

  type Primitive = Function[List[Any],Any]

  case class Str(str: String)

  case class Closure(env: Environment, params: List[String], body: List[Any]){}

  case class Cont(k:Any=>TailRec[Any])

  case class Ex(msg: String) extends Exception(msg)

  class SyntaxException(msg: String) extends Ex(msg)

  class UnboundIdentifier(msg:String) extends Ex(msg)

}
