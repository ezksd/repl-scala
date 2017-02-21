package ezksd

import scala.collection.mutable

class Environment(parent: Environment, map: mutable.Map[String, Any]) {
  def this(p: Environment) {
    this(p, mutable.Map())
  }

  protected def getMap: mutable.Map[String, Any] = map

  def lookup(key: String): Any = map.getOrElse(key, parent.lookup(key))

  def define(key: String, value: Any): String = {
    map.put(key, value)
    "define :"+key
  }

  def extend(params: mutable.Map[String, Any]): Environment = new Environment(this, params)

  def set(key: String, value: Any): Unit =
    if (map.contains(key))
      map.put(key, value)
    else
      throw new UnboundIdentifer("key")
}
