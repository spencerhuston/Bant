package Lexer

import Position.{inQuotes, hasNext, curr, peek, newline}

object Lexer {
  var position = Position
  var tokenStream: Array[Token] = Array()

  def isComment: Boolean = !inQuotes && curr == '#'

  def isTerminator = ???

  def isWhitespace: Boolean = curr == ' ' || curr == '\t'

  def isDelimiter = Operators.arithmeticOperators.contains(curr)

  def isKeyword = ???

  def isValue = ???

  def isIdent = ???

  def scanHelper() = {
    if (hasNext) {
      if (isComment) ???
      else if (isTerminator) ???
      else if (isWhitespace) ???
      else if (isDelimiter) ???
      else if (isKeyword) ???
      else if (isValue) ???
      else if (isIdent) ???
    }
  }

  def scan(sourceString: String) = {
    position.source = sourceString
    scanHelper()
  }
}