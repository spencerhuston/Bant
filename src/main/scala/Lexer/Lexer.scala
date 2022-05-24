package Lexer

import Position._
import SyntaxDefinitions._

import scala.annotation.tailrec

object Lexer {
  var position = Position
  var tokenStream: Array[Token] = Array()

  def isComment: Boolean = !inQuotes && curr == '#'

  def isTerminator: Boolean = curr == ';' || curr == '\n'

  def isWhitespace: Boolean = curr == ' ' || curr == '\t'

  def isRawDelimiter: Boolean = RawDelimiters.values.toList.contains(curr)

  def isDelimiter: Boolean = (Delimiters.values.toList ++ Operators.values.toList).contains(curr)

  def isKeyword: Boolean = Keywords.values.toList.contains(curr)

  def isNumValue: Boolean = ???

  def isIdent: Boolean = ???

  @tailrec
  def scanHelper(): Unit = {
    if (hasNext) {
      if (isValidCharacter) {
        if (isComment) skipLine()
        else if (isTerminator) {
          tokenStream = tokenStream :+ Terminator(filePositionFactory, "terminator_token_text")
          newline()
        }
        else if (isWhitespace) advanceChar()
        else if (isRawDelimiter)
          ??? // split into tokens from possible combo of delim, ident, keyword, value, etc.
        else if (isKeyword) {
          tokenStream = tokenStream :+ Keyword(filePositionFactory, "keyword_token_text")
          ??? // advance past keyword
        }
        else if (isNumValue) ???
        else if (isIdent) ???

        scanHelper()
      }
      else
        // invalid character
        println(s"Invalid character: $curr")
    }
    else
      tokenStream = tokenStream :+ EOF(filePositionFactory, "EOF")
  }

  def scan(sourceString: String) = {
    position.source = sourceString
    scanHelper()
  }
}