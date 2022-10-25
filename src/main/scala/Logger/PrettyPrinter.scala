package Logger

import Lexer.Token
import Parser.Exp

import scala.Console.{RED, RESET, YELLOW}

object PrettyPrinter {
  var expTreeString = ""

  def expTreeToString(obj: Any, depth: Int = 0, paramName: Option[String] = None): Unit = {
    // Object Header
    val indent = "  " * depth
    val prettyName = paramName.fold("")(x => s"$x: ")
    val ptype = obj match {
                  case seq: Iterable[Any] if seq.isEmpty => s"$RED" + s"Empty$RESET"
                  case _: Iterable[Any] => ""
                  case obj: Product => obj.productPrefix
                  case _ => obj.toString
                }

    expTreeString += s"$indent$YELLOW$prettyName$RESET$ptype\n"

    // Expression Type
    obj match {
      case exp: Exp =>
        expTreeString += s"${"  " * (depth + 1)}${YELLOW}expType:$RESET ${exp.expType.printType()}\n"
      case _ =>
    }

    // Recurse into object members
    obj match {
      case _: Token =>
      case seq: Iterable[Any] =>
        seq.foreach(expTreeToString(_, depth + 1))
      case obj: Product =>
        (obj.productIterator zip obj.productElementNames)
          .foreach { case (subObj, paramName) => expTreeToString(subObj, depth + 1, Some(paramName)) }
      case _ =>
    }
  }
}
