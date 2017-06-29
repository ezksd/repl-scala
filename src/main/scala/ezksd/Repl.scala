package ezksd


import ezksd.Parser.parse

object Repl {
  def main(args: Array[String]): Unit = {
    val int = new Interpreter(Environment.env0)
    while (true) {
      val txt = io.StdIn.readLine(">")
      if (txt == null || txt.equals("") || txt.equals("\u0004")) {
        return
      } else {
        try {
          parse(txt).foreach(x => int.eval(x,print))
        } catch {
          case ex: Ex => print(ex.getMessage)
        }
      }
    }
  }

}
