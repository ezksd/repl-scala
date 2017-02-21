package ezksd

import ezksd.Parser.parse

import scala.collection.mutable

object Interpreter {
  def eval(expr: Any, env: Environment): Any = {
    expr match {
      case "define" :: (key: String) :: value :: Nil => env.define(key, value)
      case "set" :: (key: String) :: value :: Nil => env.set(key, value)
      case "lambda" :: (params: List[String]) :: xs => Closure(env, params, xs)
      case "if"::pred::first::second::Nil =>{
        eval(pred,env) match {
          case true => eval(first,env)
          case false => eval(second,env)
          case _ => throw new SyntaxException("not a boolean value")
        }
      }
      case s: String => env.lookup(s)
      case fun :: vals => eval(fun, env) match {
        case Closure(envSaved, params, body) =>
          val evaledValues = vals.map(v => eval(v, env))
          val localMap = mutable.Map(params.zip(evaledValues).toMap.toSeq: _*)
          val newEnv = envSaved.extend(localMap)
          eval(body, newEnv)
      }
      case _ => expr
    }
  }

  def main(args: Array[String]): Unit = {
      val exp = parse("(define fact\n  (lambda (n)\n    (cond ((= n 1) 1)\n          (else 0))))")
    print(exp.head)
//    print(eval(exp.head,env0))

    //    exp.map(e => eval(e, env0)).foreach(print)

  }

}
