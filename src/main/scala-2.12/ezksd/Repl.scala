package ezksd

import java.io.InputStreamReader
import java.lang.Exception

import ezksd.Interpreter.evalAndPrint

import scala.util.parsing.combinator._
import ezksd.Parser.parse

import scala.util.{Failure, Success}

object Repl {
  def main(args: Array[String]): Unit = {
    while (true){
      val txt = io.StdIn.readLine(">")
      if(txt==null || txt.equals("") || txt.equals("\u0004")){
        return
      } else{
        try{
          parse(txt).foreach(evalAndPrint)
        }catch  {
          case ex:Ex => System.err.println(ex.getMessage)
        }
      }
    }
  }

}
