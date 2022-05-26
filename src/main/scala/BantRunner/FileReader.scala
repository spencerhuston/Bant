package BantRunner

import java.io.FileNotFoundException
import scala.io.Source
import scala.util.Using

object FileReader {
  def readBantSource(filepath: String): Option[String] = {
    if (filepath.takeRight(4) != ".bnt") {
      println("Error: Bant file requires .bnt extension") // add red text for logging
      return None
    }

    try {
      Some(Using(Source.fromFile(filepath)) { source => source.mkString }.get)
    } catch {
      case f: FileNotFoundException =>
        println(s"Error: $f")
        None
    }
  }
}
