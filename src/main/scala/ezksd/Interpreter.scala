package ezksd

import ezksd.Parser.parse

object Interpreter {

  def cps_map[T](l: List[Any], f: (Any, Any => T) => T, k: List[Any] => T):T = {
    l match {
      case Nil => k(Nil)
      case x::xs => f(x,head => cps_map(xs,f,tail => k(head::tail)))
    }
  }

  def evalList[T](list: List[Any], env: Environment, k: List[Any] => T) : T = {
    cps_map(list, (x:Any, k1:Any => T) => eval(x, env, r => k1(r)), k)
  }

  def evalAndPrint(expr: Any): Unit = eval(expr, Environment.env0, println)

  def firstString(list: List[Any]): String = list.head.asInstanceOf[String]

  def eval[T](expr: Any, env: Environment, cont: Any => T) : T =  {
    expr match {
      case s: String => cont(env.lookup(s))
      case "define" :: xs => xs match {
        case (key: String) :: value :: Nil => eval(value, env, r => cont(env.define(key, r)))
        case (head: List[String]) :: body => cont(env.define(head.head, Closure(env, head.tail, body)))
      }
      case "set!" :: (key: String) :: value :: Nil => eval(value, env, r =>cont(env.set(key, value)))
      case "lambda" :: (params: List[String]) :: xs => cont(Closure(env, params, xs))
      case "call/cc" :: e1 :: Nil => eval(e1, env, {
        case Closure(saved, h :: xs, body) => eval(body, saved.extend(h, Cont(cont)), cont)
      })
      case "quote" :: value :: Nil => cont(value)
      case "let" :: (list: List[List[Any]]) :: body =>
        eval(Closure(env, list.map(firstString), body) :: list.map(_.tail.head), env, cont)
      case "let" :: (name: String) :: (list: List[List[Any]]) :: body =>
        val closure = Closure(env, list.map(firstString), body)
        eval(closure :: list.map(_.tail.head), env.extend(name, closure), cont)
      case "if" :: pred :: first :: second :: Nil => eval(pred, env, {
        case true => eval(first, env, cont)
        case false => eval(second, env, cont)
      })
      case list: List[Any] => evalList(list, env, {
        case Closure(saved, p, body) :: v => evalList(body, saved.extend(p, v), r => cont(r.last))
        case Primitive(f) :: v => cont(f(v))
        case (k:Cont) :: Nil => cont(k)
        case Cont(k) :: v :: Nil => k(v).asInstanceOf[T]
//        case c@Cont(k):: v => if(v isEmpty) cont(c) else k(v.head).asInstanceOf[T]
      })
      case _ => cont(expr)
    }
  }

  def main(args: Array[String]) {
    val exp = parse("(let ((yin (call/cc (lambda (c) c)))) (display \"#\") (let ((yang (call/cc (lambda (c) c)))) (display \"*\") (yin yang)))")
    exp.foreach(e => eval(e, Environment.env0, println))
  }

}
