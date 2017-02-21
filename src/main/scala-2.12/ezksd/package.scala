
package object ezksd {

  val env0 = new Environment(null) {
    override def lookup(key: String): Any = {
      if (getMap.contains(key))
        getMap(key)
      else
        throw new UnboundIdentifer(key)
    }
  }

  case class Str(str: String)

  class ParseException(msg: String) extends Exception(msg)

  class SyntaxException(msg: String) extends Exception(msg)

  case class Closure(env: Environment, params: List[String], body: List[Any])

  class UnboundIdentifer(msg: String) extends Exception(msg)

}
