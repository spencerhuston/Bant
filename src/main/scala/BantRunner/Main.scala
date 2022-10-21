package BantRunner

import BantRunner.CmdLineParser.parseCmdLine
import BantRunner.FileReader.readBantSource
import Lexer.Lexer.scan
import Lexer.{Lexer, Token}
import Logger.Level
import Logger.Logger.{ERROR, LOG, LOG_HEADER, WARN, setLevel}
import Logger.PrettyPrinter.astToString
import Parser.Parser.parse

import scala.collection.mutable.ArrayBuffer

object Main {
  def getSource(args: Array[String]): Option[String] = {
    parseCmdLine(args) match {
      case Some(config) =>
        LOG(Level.INFO, s"Config: $config")
        setLevel(config.logLevel)
        readBantSource(config.filepath) match {
          case Some(bantSource) => Some(bantSource)
          case _ => None
        }
      case _ =>
        None
    }
  }

  def run(args: Array[String]): Unit = {
    val tokenStream = getSource(args) match {
      case Some(bantSource) =>
        LOG_HEADER("SOURCE", bantSource)
        scan(bantSource)
      case _ => ArrayBuffer[Token]()
    }

    if (tokenStream.size == 1) {
      WARN("Warning: Empty source file")
    } else if (Lexer.errorOccurred) {
      ERROR("Error(s) occurred, stopping run")
    } else {
      var rootExp = parse(tokenStream)
      if (Parser.Parser.warnings > 0)
        WARN(s"${Parser.Parser.warnings} warnings occurred")
      if (Parser.Parser.numErrors > 0)
        ERROR(s"${Parser.Parser.numErrors} errors occurred")
      LOG_HEADER("Untyped AST", astToString(rootExp))
    }
  }

  def clear(): Unit = {
    Lexer.clear()
  }

  def main(args: Array[String]): Unit = {
    LOG(Level.INFO, "Bant Launched")
    run(args)
    LOG(Level.INFO, "Bant Finished")
    clear()
  }
}