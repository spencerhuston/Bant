package BantRunner

import BantRunner.CmdLineParser.parseCmdLine
import BantRunner.FileReader.readBantSource

import scala.sys.exit

object Main {
  def main(args: Array[String]) = {
    parseCmdLine(args) match {
      case Some(config) =>
        readBantSource(config.filepath) match {
          case Some(bantSource) => println(bantSource)
          case _ => None
        }
      case _ =>
        None
    }
  }
}