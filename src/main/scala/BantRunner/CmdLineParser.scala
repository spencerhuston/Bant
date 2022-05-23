package BantRunner

import scopt.OParser

object CmdLineParser {
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
}
