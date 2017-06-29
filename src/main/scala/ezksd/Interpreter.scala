package ezksd

import ezksd.Parser.parse

class Interpreter(env:Environment) {

  def cps_map[T](l: List[Any], f: (Any, Any => T) => T, k: List[Any] => T):T = {
    l match {
      case Nil => k(Nil)
      case x::xs => f(x,head => cps_map(xs,f,tail => k(head::tail)))
    }
  }

  def evalList[T](list: List[Any], k: List[Any] => T) : T = {
    cps_map(list, (x:Any, k1:Any => T) => eval(x, r => k1(r)), k)
  }

  def firstString(list: List[Any]): String = list.head.asInstanceOf[String]

  def eval[T](expr: Any, cont: Any => T) : T =  {
    expr match {
      case s: String => cont(env.lookup(s))
      case "define" :: xs => xs match {
        case (key: String) :: value :: Nil => eval(value, r => cont(env.define(key, r)))
        case (head: List[String]) :: body => cont(env.define(head.head, Closure(env, head.tail, body)))
      }
      case "set!" :: (key: String) :: value :: Nil => eval(value, r => cont(env.set(key, r)))
      case "lambda" :: (params: List[String]) :: xs => cont(Closure(env, params, xs))
      case "call/cc" :: e1 :: Nil => eval(e1, {
        case Closure(saved, h :: xs, body) => new Interpreter(saved.extend(h,Cont(cont))).eval(body, cont)
      })
      case "quote" :: value :: Nil => cont(value)
      case "let" :: (list: List[List[Any]]) :: body =>
        eval(Closure(env, list.map(firstString), body) :: list.map(_.tail.head), cont)
      case "let" :: (name: String) :: (list: List[List[Any]]) :: body =>
        val closure = Closure(env, list.map(firstString), body)
        new Interpreter(env.extend(name, closure)).eval(closure :: list.map(_.tail.head), cont)
      case "if" :: pred :: first :: second :: Nil => eval(pred, {
        case true => eval(first, cont)
        case false => eval(second, cont)
      })
      case list: List[Any] => evalList(list, {
        case Closure(saved, p, body) :: v => new Interpreter(saved.extend(p,v)).evalList(body, r => cont(r.last))
        case Primitive(f) :: v => cont(f(v))
        case (k:Cont) :: Nil => cont(k)
        case Cont(k) :: v :: Nil => k(v).asInstanceOf[T]
      })
      case _ => cont(expr)
    }
  }

}
object Interpreter{

  def main(args: Array[String]) {
    val exp = parse("(let ((yin (call/cc (lambda (c) c)))) (display \"#\") (let ((yang (call/cc (lambda (c) c)))) (display \"*\") (yin yang)))")
    exp.foreach(e => new Interpreter(Environment.env0).eval(e, println))
  }
}
