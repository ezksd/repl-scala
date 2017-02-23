package ezksd

import ezksd.Parser.parse

import scala.annotation.tailrec
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

  //  @tailrec
  def eval(expr: Any, env: Environment, k: Any => Unit): Unit = {
    expr match {
      //      case "define" :: (key: String) :: value :: Nil => eval(value, env, r => k(env.define(key, r)))
      case s: String => k(env.lookup(s))
      case "define" :: xs => xs match {
        case (key: String) :: value :: Nil => eval(value, env, r => k(env.define(key, r)))
        //        case ((key: String) :: (params: List[String])) :: body => k(env.define(key, Closure(env, params, body)))
        case (head: List[String]) :: body => k(env.define(head.head, Closure(env, head.tail, body)))
        case _ => throw new SyntaxException("illegal define statement...")
      }
      case "set!" :: (key: String) :: value :: Nil => k(env.set(key, value))
      case "lambda" :: (params: List[String]) :: xs => k(Closure(env, params, xs))
      case "call/cc" :: e1 :: Nil => eval(e1, env, {
        case Closure(saved, params, body) => eval(body, saved.extend(params.head, k), k)
      })
      case "quote"::value::Nil=> k(value)
      case "let" :: xs => xs match {
        case (list: List[List[Any]]) :: body =>
          val closure = Closure(env, list.map(_.head).map(_.asInstanceOf[String]), body)
          cps_map(list.map(_.tail.head), evalHelper(env), evaluated => eval(closure :: evaluated, env, k))
        case (s: String) :: (list: List[List[Any]]) :: body =>
          val closure = Closure(env, list.map(_.head.asInstanceOf[String]), body)
          env.define(s, closure)
          cps_map(list.map(_.tail.head), evalHelper(env), evaluated => eval(closure :: evaluated, env, k))
      }
      case "if" :: pred :: first :: second :: Nil => eval(pred, env, {
        case true => eval(first, env, k)
        case false => eval(second, env, k)
      })
      case fun :: vals => eval(fun, env, r1 => {
        cps_map(vals, evalHelper(env), evaluated =>
          r1 match {
            case Closure(saved, params, body) =>
              cps_map(body, evalHelper(saved.extend(params, evaluated)),
                r2 => k(r2.last))
            case prim: Primitive => k(prim(evaluated))
            case f: Function[Any, Unit] => if (evaluated.isEmpty) k(f) else f(evaluated.head)
          })
      })
      case _ => k(expr)
    }
  }

  def main(args: Array[String]): Unit = {
    val exp = parse("(cdr '(1 2 3 4 5))")
    exp.foreach(e => eval(e, env0, println))

  }


}
