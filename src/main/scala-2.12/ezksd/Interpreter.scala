package ezksd

import ezksd.Parser.parse

import scala.collection.mutable


object Interpreter {
  val env0 = new Environment(Primitives.init) {
    override def lookup(key: String): Any = {
      if (getMap.contains(key))
        getMap(key)
      else
        throw new UnboundIdentifer(key)
    }
  }

  def cps_map(list: List[Any], f: (Any, Any => Unit) => Unit, k: List[Any] => Unit): Unit = {
    if (list.isEmpty)
      k(List.empty)
    else
      f(list.head, newhead => cps_map(list.tail, f, newtail => k(newhead :: newtail)))
  }

  def evalHelper(env: Environment): ((Any, Any => Unit) => Unit) = (x, k1) => eval(x, env, r => k1(r))

  def evalAndPrint(expr: Any): Unit = eval(expr, env0, println)

  def eval(expr: Any, env: Environment, k: Any => Unit): Unit = {
    expr match {
      //      case "define" :: (key: String) :: value :: Nil => eval(value, env, r => k(env.define(key, r)))
      case "define" :: xs => xs match {
        case (key: String) :: value :: Nil => eval(value, env, r => k(env.define(key, r)))
        case ((key: String) :: (params: List[String])) :: body => k(env.define(key, Closure(env, params, body)))
        case _ => throw new SyntaxException("illegal define statement...")
      }
      case "set!" :: (key: String) :: value :: Nil => k(env.set(key, value))
      case "lambda" :: (params: List[String]) :: xs => k(Closure(env, params, xs))
      case "call/cc" :: e1 :: Nil => eval(e1, env, {
        case Closure(saved, params, body) => eval(body, saved.extend(mutable.Map[String, Any]((params.head, k))), k)
      })
      case "if" :: pred :: first :: second :: Nil => eval(pred, env, {
        case true => eval(first, env, k)
        case false => eval(second, env, k)
      })
      case s: String => k(env.lookup(s))
      case fun :: vals => eval(fun, env, r1 => {
        cps_map(vals, evalHelper(env), evaluated =>
          r1 match {
            case Closure(saved, params, body) =>
              cps_map(body, evalHelper(saved.extend(mutable.Map(params.zip(evaluated).toMap.toSeq: _*))),
                r2 => k(r2.last))
            case prim: Primitive => k(prim(evaluated))
            case f:Function[Any,Unit] => f(evaluated.head)
          })
      })
      case _ => k(expr)
    }
  }

  def main(args: Array[String]): Unit = {
    val exp = parse("(define a 1)")
    exp.foreach(e => eval(e, env0, println))

  }

}
