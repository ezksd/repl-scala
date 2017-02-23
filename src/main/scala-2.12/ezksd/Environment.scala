package ezksd

import scala.collection.mutable

class Environment(parent: Environment, map: mutable.Map[String, Any]) {
  def this(m: mutable.Map[String, Any]) {
    this(null, m)
  }

  def lookup(key: String): Any = map.getOrElse(key, parent.lookup(key))

  def define(key: String, value: Any): String = {
    map.put(key, value)
    "define :" + key + " done ..."
  }

  def extend(params: List[String], values: List[Any]): Environment = extend(mutable.Map(params.zip(values).toMap.toSeq: _*))

  def extend(param: String, value: Any): Environment = extend(mutable.Map[String, Any]((param, value)))

  def extend(map: mutable.Map[String, Any]): Environment = new Environment(this, map)

  def set(key: String, value: Any): String = {
    if (map.contains(key))
      map.put(key, value)
    else
      throw new UnboundIdentifer("unboud identifer : key")
    "set:" + key + " done..."
  }

  protected def getMap: mutable.Map[String, Any] = map

}


