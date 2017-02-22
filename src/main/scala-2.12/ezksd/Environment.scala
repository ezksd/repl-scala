package ezksd

import scala.collection.mutable

class Environment(parent: Environment, map: mutable.Map[String, Any]) {
  def this(m:mutable.Map[String,Any]) {
    this(null,m)
  }

  protected def getMap: mutable.Map[String, Any] = map

  def lookup(key: String): Any = map.getOrElse(key, parent.lookup(key))

  def define(key: String, value: Any): String = {
    map.put(key, value)
    "define :"+key +" done ..."
  }

  def extend(params: mutable.Map[String, Any]): Environment = new Environment(this, params)

  def set(key: String, value: Any): String ={
    if (map.contains(key))
      map.put(key, value)
    else
      throw new UnboundIdentifer("unboud identifer : key")
    "set:" +key +" done..."
  }

}


