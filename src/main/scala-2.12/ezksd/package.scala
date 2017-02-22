
package object ezksd {

  case class Str(str: String)

  abstract class Primitive() extends Function[List[Any], Any]

  class ParseException(msg: String) extends Exception(msg)

  class SyntaxException(msg: String) extends Exception(msg)

  case class Closure(env: Environment, params: List[String], body: List[Any])

  class UnboundIdentifer(msg: String) extends Exception(msg)

}
