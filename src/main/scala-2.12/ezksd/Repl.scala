package ezksd

import ezksd.Interpreter.evalAndPrint

import ezksd.Parser.parse

object Repl {
  def main(args: Array[String]): Unit = {
    while (true) {
      val txt = io.StdIn.readLine(">")
      if (txt == null || txt.equals("") || txt.equals("\u0004")) {
        return
      } else {
        try {
          parse(txt).foreach(evalAndPrint)
        } catch {
          case ex: Ex => print(ex.getMessage)
        }
      }
    }
  }

}
