package Lexer

import Logger.Logger.lineList

object Position {
  var index = 0
  var lineNumber = 0
  var columnNumber = 0
  var lineText = ""
  var inQuotes = false

  var source: String = ""

  def hasNext: Boolean = index != source.length
  def curr: Char = source(index)
  def next: Char = source(index + 1)
  def peek(): Option[Char] = if (index + 1 != source.length) Some(next) else None

  def advanceChar(): Unit = {
    curr match {
      case ' ' =>
        lineText += " "
        columnNumber += 1
        index += 1
      case '\t' =>
        lineText += "\t"
        columnNumber += 4
        index += 4
      case _ =>
        lineText += curr
        columnNumber += 1
        index += 1
    }
  }

  def resetLine(): Unit = {
    lineNumber += 1
    columnNumber = 0
    lineText = ""
  }

  def skipLine(): Unit = {
    index = source.indexOf("\n", index) + 1
    resetLine()
  }

  def newline(semicolon: Boolean = false): Unit = {
    index += (if (semicolon) 2 else 1)
    resetLine()
  }

  def filePositionFactory: FilePosition = {
    FilePosition(lineNumber, columnNumber, lineList(lineNumber))
  }

  def clear(): Unit = {
    index = 0
    lineNumber = 0
    columnNumber = 0
    lineText = ""
    inQuotes = false

    source = ""
    lineList = Array[String]()
  }
}
