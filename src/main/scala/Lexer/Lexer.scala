package Lexer

import Position.{curr, hasNext, inQuotes, newline, peek}
import SyntaxDefinitions.{RawDelimiters, Delimiters, Operators, Keywords}

object Lexer {
  var position = Position
  var tokenStream: Array[Token] = Array()

  def isComment: Boolean = !inQuotes && curr == '#'

  def isTerminator: Boolean = curr == ';' || curr == '\n'

  def isWhitespace: Boolean = curr == ' ' || curr == '\t'

  def isRawDelimiter: Boolean = RawDelimiters.values.toList.contains(curr)

  def isDelimiter: Boolean = (Delimiters.values.toList ++ Operators.values.toList).contains(curr)

  def isKeyword: Boolean = ???

  def isNumValue: Boolean = ???

  def isIdent: Boolean = ???

  def scanHelper() = {
    if (hasNext) {
      if (isComment) ???
      else if (isTerminator) ???
      else if (isWhitespace) ???
      else if (isDelimiter) ???
      else if (isKeyword) ???
      else if (isNumValue) ???
      else if (isIdent) ???
    }
  }

  def scan(sourceString: String) = {
    position.source = sourceString
    scanHelper()
  }
}