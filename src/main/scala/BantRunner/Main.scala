package BantRunner

import scala.sys.exit
import scala.io.Source
import scopt.OParser

import java.io.FileNotFoundException

object Main {
  case class ParserConfig(
                           filepath: String = "",
                     debug: Boolean = false
                   )

  def main(args: Array[String]) = {
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

    OParser.parse(cmdLineParser, args, ParserConfig()) match {
      case Some(config) =>
        if (config.filepath.takeRight(4) != ".bnt") {
          println("Error: Bant file requires .bnt extension") // add red text for logging
          exit(2)
        }

        try {
          val bantProgramSource = Source.fromFile(config.filepath).getLines.toList
          println(bantProgramSource)
        } catch {
          case f: FileNotFoundException => println(s"Error: Could not find Bant source file ${config.filepath}")
            exit(3)
        }
      case _ =>
        exit(1)
    }
  }
}