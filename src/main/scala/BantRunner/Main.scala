package BantRunner

import scala.sys.exit
import scopt.OParser

import scala.io.Source
import java.io.FileNotFoundException

object Main {
  case class ParserConfig(
                           filepath: String = "",
                           debug: Boolean = false
                         )

  def parseCmdLine(args: Array[String]) = {
    val builder = OParser.builder[ParserConfig]
    val cmdLineParser = {
      import builder._
      OParser.sequence(
        programName("Bant Language"),
        opt[String]('f', "file")
          .required()
          .valueName("<file>")
          .action((x, c) => c.copy(filepath = x))
          .text("Bant file, required extension is .bnt"),
        opt[Unit]('d', "debug")
          .action((_, c) => c.copy(debug = true))
          .text("Enable debug mode"),
        help("help").text("prints this usage text")
      )
    }

    OParser.parse(cmdLineParser, args, ParserConfig())
  }

  def readBantSource(filepath: String): Option[String] = {
    if (filepath.takeRight(4) != ".bnt") {
      println("Error: Bant file requires .bnt extension") // add red text for logging
      return None
    }

    try {
      Some(Source.fromFile(filepath).mkString)
    } catch {
      case f: FileNotFoundException => println(s"Error: Could not find Bant source file ${filepath}")
        None
    }
  }

  def main(args: Array[String]) = {
    args.foreach { println(_) }
    parseCmdLine(args) match {
      case Some(config) =>
        readBantSource(config.filepath) match {
          case Some(bantSource) => println(bantSource)
          case _ => exit(1)
        }
      case _ =>
        exit(1)
    }
  }
}