package ezksd

import scala.util.control.TailCalls.{TailRec, done, tailcall}

object Interpreter{

  def map(l: List[Any], f: (Any, Any => TailRec[Any]) => TailRec[Any], k: List[Any] => TailRec[Any]): TailRec[Any] =
    if (l.isEmpty) k(l) else f(l.head, head => map(l.tail, f, tai => k(head :: tai)))

  def evalList(list: List[Any], env: Environment, k: List[Any] => TailRec[Any]): TailRec[Any] =
    map(list, (x, k1: Any => TailRec[Any]) => tailcall(eval(x, env, r => k1(r))), k)

  def first(list: List[Any]): String = list.head.asInstanceOf[String]

  def eval(expr: Any, env: Environment, k: Any => TailRec[Any]): TailRec[Any] =
    expr match {
      case s: String => k(env.lookup(s))
      case "define" :: xs => xs match {
        case (key: String) :: value :: _ => tailcall(eval(value, env, r => k(env.define(key, r))))
        case (head: List[String]) :: body => k(env.define(head.head, Closure(env, head.tail, body)))
      }
      case "set!" :: (key: String) :: value :: _ => tailcall(eval(value, env, r => k(env.set(key, value))))
      case "lambda" :: (params: List[String]) :: xs => k(Closure(env, params, xs))
      case "call/cc" :: e::_ => tailcall(eval(List(e,Cont(k)),env,k))
      case "quote" :: v :: _ => k(v)
      case "let" :: (list: List[List[Any]]) :: body =>
        tailcall(eval(Closure(env, list.map(first), body) :: list.map(_.tail.head), env, k))
      case "let" :: (name: String) :: (list: List[List[Any]]) :: body =>
        val closure = Closure(env, list.map(first), body)
        tailcall(eval(closure :: list.map(_.tail.head), env.extend(name, closure), k))
      case "if" :: pred :: first :: second :: _ =>tailcall( eval(pred, env, {
        case true => tailcall(eval(first, env, k))
        case false => tailcall(eval(second, env, k))
      }))
      case list: List[Any] => tailcall(evalList(list, env, {
        case Closure(saved, p, body) :: v => tailcall(evalList(body, saved.extend(p, v), l => k(l.last)))
        case (prim: Primitive) :: v => k(prim(v))
        case Cont(k0)::v::Nil => k0(v)
      }))
      case _ => k(expr)
    }

  def eval(expr: Any): Any = eval(expr, Environment.env0, done).result


  def main(args: Array[String]) {
    Parser.parsetest("(let ((yin " +
                       "((lambda (cc) (display \"@\") cc) (call/cc (lambda (c) c)))) " +
                       "(yang ((lambda (cc) (display \"*\") cc) (call/cc (lambda (c) c)))))" +
                       "  (yin yang))")
      .foreach(x=>print(eval(x)))
  }

}
