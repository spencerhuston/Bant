package BantRunner

import BantRunner.CmdLineParser.parseCmdLine
import BantRunner.FileReader.readBantSource
import Lexer.Lexer.scan
import Lexer.Token

import scala.collection.mutable.ArrayBuffer

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

  def run(args: Array[String]): Boolean = {
    val tokenStream = getSource(args) match {
      case Some(bantSource) =>
        println(bantSource)
        scan(bantSource)
      case _ => ArrayBuffer[Token]()
    }

    tokenStream.nonEmpty // change as phases are added
  }

  def clear(): Unit = {
    Lexer.Lexer.clear()
  }

  def main(args: Array[String]): Unit = {
    run(args)
    clear()
  }
}