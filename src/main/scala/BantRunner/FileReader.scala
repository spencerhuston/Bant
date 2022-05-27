package BantRunner

import Logger.Level
import Logger.Logger.{ERROR, LOG}

import java.io.FileNotFoundException
import scala.io.Source
import scala.util.Using

object FileReader {
  def readBantSource(filepath: String): Option[String] = {
    LOG(Level.INFO, "Checking source file extension")
    if (filepath.takeRight(4) != ".bnt") {
      ERROR("Error: Bant file requires .bnt extension") // add red text for logging
      return None
    }

    try {
      LOG(Level.INFO, s"Attempting to read source file: $filepath")
      Some(Using(Source.fromFile(filepath)) { source => source.mkString }.get)
    } catch {
      case f: FileNotFoundException =>
        LOG(Level.ERROR, s"Failed to open $filepath")
        ERROR(s"Error: $f")
        None
    }
  }
}
