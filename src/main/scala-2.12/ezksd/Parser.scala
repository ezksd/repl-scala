package ezksd

import ezksd.Interpreter.eval

import scala.io.StdIn
import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  def number: Parser[Int] = """-?\d+(\.\d*)?""".r ^^ { _.toInt }

  def symbol: Parser[String] =
    """[^\s\r,"'()]+""".r ^^ {
      _.toString
    }

  def string: Parser[Str] = "\"" ~> """[^\s\r",'()]*""".r <~ "\"" ^^ Str

  def bool: Parser[Boolean] = "#" ~> "t|f".r ^^ {
    "t".equals(_)
  }

  def list: Parser[List[Any]] = "(" ~> rep(expr) <~ ")"

  def quote: Parser[List[Any]] = "\'" ~> expr ^^ {
    List("quote", _)
  }

  def expr: Parser[Any] = bool | number | string | symbol | list | quote

  def program: Parser[List[Any]] = opt("""[\s\r\t]*"""".r) ~> rep(expr)

  def parse(txt: String) = parseAll(program, txt)

  def parsetest(txt:String) = parseAll(program,txt) match {
    case Success(x,_) => x
  }

  def main(args: Array[String]): Unit = {
    var txt = ""
    def continue = txt += StdIn.readLine()
    def reset  = txt  = StdIn.readLine("> ")

    reset
    while (!(txt.equals("") || txt.equals("\u0004"))) {
      try{
        parse(txt) match {
          case Success(r, _) => r map eval foreach println;reset
          case Failure(_, _) => continue
          case Error(msg, _) => print(msg)
        }
      } catch {
        case Ex(msg) => println(msg);reset
      }
    }
    print("exit...")
  }
}

