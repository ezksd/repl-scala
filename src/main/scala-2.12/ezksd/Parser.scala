package ezksd

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  def number: Parser[Double] = """-?\d+(\.\d*)?""".r ^^ { _.toDouble }

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

  def parse(txt: String): List[Any] = parseAll(program, txt) match {
    case Success(r, _) => r
    case Failure(msg, _) => throw new ParseException(msg)
    case Error(msg, _) => throw new ParseException(msg)
  }

}

