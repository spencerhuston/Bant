package Logger

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.Console.{RED, RESET, YELLOW}

object Level extends Enumeration {
  val NONE: Level.Value = Value("NONE")
  val INFO: Level.Value = Value("INFO")
  val WARN: Level.Value = Value("WARN")
  val ERROR: Level.Value = Value("ERROR")
  val DEBUG: Level.Value = Value("DEBUG")
}

object Logger {
  var level: Level.Value = Level.NONE
  val header = "========================="
  var lineList: Array[String] = Array[String]()

  def setLevel(logLevel: String): Unit = {
    Level.values.find((levelValue: Level.Value) => levelValue.toString == logLevel) match {
      case None => level = Level.NONE
      case levelValue => level = levelValue.get
    }
  }

  def LOG(logLevel: Level.Value, str: String*)(implicit fileName: sourcecode.FileName, line: sourcecode.Line): Unit = {
    if (logLevel <= level) {
      val datetimeString = DateTimeFormatter.ofPattern("HH:mm:ss.SSS").format(LocalDateTime.now()) + " | "
      val logLevelString = logLevel.toString

      var logString = ""
      str.foreach(logString += _)

      if (logLevel == Level.DEBUG)
        logString += " | [" + fileName.value + ":" + line.value + "]"

      print(datetimeString)
      logLevelString match {
        case "WARN" => Console.print(f"${YELLOW}WARN$RESET%5s")
        case "ERROR" => Console.print(f"${RED}WARN$RESET%5s")
        case _ => print(f"$logLevelString%5s")
      }
      println(" | " + logString)
    }
  }

  def LOG_HEADER(title: String, str: String): Unit = {
    if (level == Level.DEBUG) {
      println(header)
      println(title)
      println(header)
      println(str)
      println(header)
    }
  }

  def WARN(str: String): Unit = Console.println(s"${YELLOW}$str$RESET")
  def ERROR(str: String): Unit = Console.println(s"${RED}$str$RESET")
}
