package BantRunner

import BantRunner.CmdLineParser.parseCmdLine
import BantRunner.FileReader.readBantSource
import Lexer.Lexer.scan
import Lexer.{Lexer, Token}
import Logger.Level
import Logger.Logger.{ERROR, LOG, LOG_HEADER, WARN, setLevel}
import Logger.PrettyPrinter.{expTreeString, expTreeToString, printAST}
import Parser.Exp
import Parser.Parser.parse
import SemanticAnalyzer.SemanticAnalyzer.eval

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

  def generateTokenStream(args: Array[String]): ArrayBuffer[Token] = {
    getSource(args) match {
      case Some(bantSource) =>
        LOG_HEADER("SOURCE", bantSource)
        scan(bantSource)
      case _ => ArrayBuffer[Token]()
    }
  }

  def runParser(tokenStream: ArrayBuffer[Token]): Exp = {
    val untypedRootExp = parse(tokenStream)
    if (Parser.Parser.warnings > 0)
      WARN(s"${Parser.Parser.warnings} warnings occurred")
    if (Parser.Parser.numErrors > 0)
      ERROR(s"${Parser.Parser.numErrors} errors occurred")
    expTreeToString(untypedRootExp)
    LOG_HEADER("Untyped AST", expTreeString)
    untypedRootExp
  }

  def runSemanticAnalyzer(exp: Exp): Exp = {
    val typedRootExp = eval(exp)
    LOG_HEADER("Typed AST", printAST(typedRootExp))
    typedRootExp
  }

  def run(args: Array[String]): Unit = {
    val tokenStream = generateTokenStream(args)
    if (tokenStream.size == 1) {
      WARN("Warning: Empty source file")
      return
    } else if (Lexer.errorOccurred) {
      ERROR("Error(s) occurred, stopping run")
      return
    }

    val untypedRootExp = runParser(tokenStream)
    if (Parser.Parser.numErrors > 0)
      return

    val typedRootExp = runSemanticAnalyzer(untypedRootExp)
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