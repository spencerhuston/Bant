package Lexer

import java.lang.Character
import SyntaxDefinitions.RawDelimiters

object Position {
  var index = 0
  var lineNumber = 0
  var columnNumber = 0
  var lineText = ""
  var tokenText = ""
  var inQuotes = false

  var source: String = ""

  def hasNext: Boolean = index != source.length
  def curr: Char = source(index)
  def peek(): Option[Char] = if (hasNext) Some(curr) else None
  def skipLine(): Unit = index = source.indexOf("\n", index)

  def isValidCharacter: Boolean = RawDelimiters.values.toList.contains(curr) ||
                                    Character.isLetterOrDigit(curr) ||
                                    Character.isWhitespace(curr) ||
                                    curr == '#'

  def advanceChar(): Unit = {
    curr match {
      case ' ' =>
        index += 1
        columnNumber += 1
        lineText += " "
      case '\t' =>
        index += 4
        columnNumber += 4
        lineText += "\t"
      case _ =>
        index += 1
        columnNumber += 1
        lineText += curr
    }
  }

  def newline(): Unit = {
    index += 1
    lineNumber += 1
    columnNumber = 0
    lineText = ""
    tokenText = ""
  }

  def filePositionFactory = {
    FilePosition(lineNumber, columnNumber, lineText)
  }
}
