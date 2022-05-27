package BantRunner

import BantRunner.CmdLineParser.parseCmdLine
import BantRunner.FileReader.readBantSource
import Lexer.Lexer.scan
import Lexer.{Lexer, Token}
import Logger.Logger._

import scala.collection.mutable.ArrayBuffer

object Main {
  def getSource(args: Array[String]): Option[String] = {
    parseCmdLine(args) match {
      case Some(config) =>
        setLevel(config.logLevel)
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
        LOG_HEADER("SOURCE", bantSource)
        scan(bantSource)
      case _ => ArrayBuffer[Token]()
    }

    tokenStream.nonEmpty && !Lexer.errorOccurred
  }

  def clear(): Unit = {
    Lexer.clear()
  }

  def main(args: Array[String]): Unit = {
    run(args)
    clear()
  }
}