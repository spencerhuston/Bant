package Lexer

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
  def advance = ???

  def newline(): Unit = {
    index += 1
    lineNumber += 1
    columnNumber = 0
    lineText = ""
    tokenText = ""
  }
}
