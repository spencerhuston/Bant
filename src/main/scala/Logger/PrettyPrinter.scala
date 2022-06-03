package Logger

import Parser._
import TypeChecker.Type

import scala.collection.mutable.ArrayBuffer

import scala.Console.{RESET, YELLOW}

object PrettyPrinter {
  def astToString(exp: Exp, depth: Int = 0): String = {
    exp match {
      case e: Product =>
        s"${" " * depth}${e.productPrefix}(\n" +
        s"${(e.productElementNames.toList.takeRight(e.productElementNames.length - 1) zip
          e.productIterator.toList.takeRight(e.productIterator.length - 1)).
          map(elem =>
            s"${" " * (depth + 1)}$YELLOW${elem._1}$RESET: ${
              elem._2 match {
                case t: Type => t.toString
                case b: Boolean => b
                case i: Int => i
                case s: String => s
                case value: Val => value.toString
                case exp: Exp => "\n" + astToString(exp, depth + 1)
                case ab: ArrayBuffer[Exp] => ab.map(x => "\n" + astToString(x, depth + 1)).foldLeft("")(_ + _)
                case cp: CasePattern => cp.toString
                case vp: ValueCasePattern => vp.toString
              }
            }\n").foldLeft("")(_ + _)}" +
        s"${" " * depth})"
    }
  }
}
