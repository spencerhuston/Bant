package BantRunner

import BantRunner.CmdLineParser.parseCmdLine
import BantRunner.FileReader.readBantSource
import Lexer.Lexer.scan

object Main {
  def getSource(args: Array[String]): Option[String] = {
    parseCmdLine(args) match {
      case Some(config) =>
        readBantSource(config.filepath) match {
          case Some(bantSource) => Some(bantSource)
          case _ => None
        }
      case _ =>
        None
    }
  }

  def main(args: Array[String]): Unit = {
    getSource(args) match {
      case Some(bantSource) =>
        println(bantSource)
        scan(bantSource)
      case _ => ()
    }
  }
}