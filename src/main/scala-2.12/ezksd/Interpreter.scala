package ezksd

import ezksd.Parser.parse


object Interpreter {

  @inline
  def cps_map(l: List[Any], f: (Any, Any => Unit) => Unit, k: List[Any] => Unit): Unit = {
    if (l.isEmpty)
      k(l)
    else
      f(l.head, head => cps_map(l.tail, f, tai => k(head :: tai)))
  }

  @inline
  def evalList(list: List[Any], env: Environment, k: List[Any] => Unit): Unit = cps_map(list, (x, k1) => eval(x, env, r => k1(r)), k)

  def evalAndPrint(expr: Any): Unit = eval(expr, Environment.env0, println)

  @inline
  def firstString(list: List[Any]): String = list.head.asInstanceOf[String]

  def eval(expr: Any, env: Environment, cont: Any => Unit) {
    expr match {
      case s: String => cont(env.lookup(s))
      case "define" :: xs => xs match {
        case (key: String) :: value :: Nil => eval(value, env, r => env.define(key, r))
        case (head: List[String]) :: body => env.define(head.head, Closure(env, head.tail, body))
      }
      case "set!" :: (key: String) :: value :: Nil => env.set(key, value)
      case "lambda" :: (params: List[String]) :: xs => cont(Closure(env, params, xs))
      case "call/cc" :: e1 :: Nil => eval(e1, env, { case Closure(saved, params, body) => eval(body, saved.extend(params.head, cont), cont) })
      case "quote" :: value :: Nil => cont(value)
      case "let" :: (list: List[List[Any]]) :: body =>
        eval(Closure(env, list.map(firstString), body) :: list.map(_.tail.head), env, cont)
      case "let" :: (name: String) :: (list: List[List[Any]]) :: body =>
        val closure = Closure(env, list.map(firstString), body)
        eval(closure :: list.map(_.tail.head), env.extend(name,closure), cont)
      case "if" :: pred :: first :: second :: Nil => eval(pred, env, {
        case true => eval(first, env, cont)
        case false => eval(second, env, cont)
      })
      case list: List[Any] => evalList(list, env, {
        case Closure(saved, p, body) :: v => evalList(body, saved.extend(p, v), r => cont(r.last))
        case (prim: Primitive) :: v => cont(prim(v))
        case (f: Function[Any, Unit]) :: v => if (v.isEmpty) cont(f) else f(v.head)
      })
      case _ => cont(expr)
    }
  }

  def main(args: Array[String]): Unit = {
    val exp = parse("(let ((yin (call/cc (lambda (c) c)))) (display \"#\") (let ((yang (call/cc (lambda (c) c)))) (display \"*\") (yin yang)))")
    exp.foreach(e => eval(e, Environment.env0, println))
  }

}
