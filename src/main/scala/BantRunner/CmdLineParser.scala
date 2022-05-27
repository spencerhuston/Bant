package BantRunner

import scopt.OParser

object CmdLineParser {
  case class ParserConfig(
                           filepath: String = "",
                           logLevel: String = ""
                         )

  def parseCmdLine(args: Array[String]): Option[ParserConfig] = {
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
        opt[String]('l', "level")
          .valueName("<logLevel>")
          .action((x, c) => c.copy(logLevel = x))
          .text("Log level: INFO, WARN, ERROR, DEBUG"),
        help("help").text("prints this usage text")
      )
    }

    OParser.parse(cmdLineParser, args, ParserConfig())
  }
}
