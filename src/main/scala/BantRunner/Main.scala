package BantRunner

import BantRunner.CmdLineParser.parseCmdLine
import BantRunner.FileReader.readBantSource
import Lexer.Lexer.scan
import Lexer.{Lexer, Token}
import Logger.Level
import Logger.Logger.{ERROR, LOG, LOG_HEADER, WARN, setLevel}

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
    }

    // parser phase
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