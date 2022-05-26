package Lexer

import Position._
import SyntaxDefinitions._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Lexer {
  var position: Position.type = Position
  var tokenStream: ArrayBuffer[Token] = ArrayBuffer[Token]()

  def addToken[T <: Token](token: T): Unit = {
    tokenStream += token
  }

  def isValueOf(str: String, enum: Enumeration): Boolean = {
    enum.values.find(_.toString == str) match {
      case None => false
      case _ => true
    }
  }

  def isValidCharacter: Boolean = isValueOf(curr.toString, RawDelimiters) ||
                                  Character.isLetterOrDigit(curr) ||
                                  Character.isWhitespace(curr) ||
                                  curr == '#' || curr == '_'

  def isComment: Boolean = !inQuotes && curr == '#'

  def isTerminator: Boolean = curr == ';' || curr == '\n'

  def isWhitespace: Boolean = curr == ' ' || curr == '\t'

  def isRawDelimiter: Boolean = isValueOf(curr.toString, RawDelimiters)

  def isDelimiter: Boolean = isValueOf(curr.toString, Delimiters)

  def isNumValue: Boolean = curr.isDigit

  def isKeyword(str: String): Boolean = isValueOf(str, Keywords)

  def isIdent(str: String): Boolean = {
    val identRegex = """(^[a-zA-z_][a-zA-z\d_]*$$)""".r

    try {
      str match {
        case identRegex(_) => true
      }
    } catch {
      case _ => false
    }
  }

  @tailrec
  def handleQuotes(str: String = ""): Boolean = {
    if (hasNext) {
      if (!inQuotes && (curr == '\'' || curr == '\"')) {
        inQuotes = true
        val tmpStr = str + curr.toString
        advanceChar()
        handleQuotes(tmpStr)
      } else if (inQuotes && (curr != '\'' && curr != '\"')) {
        val tmpStr = str + curr.toString
        advanceChar()
        handleQuotes(tmpStr)
      } else if (inQuotes && (curr == '\'' || curr == '\"')) {
        addToken(Value(str + curr.toString))
        inQuotes = false
        advanceChar()

        hasNext
      }
      else true
    }
    else false
  }

  def handleDelimiter(): Unit = {
    def nextIsDelimiter: Boolean = {
      peek() match {
        case Some(char) => isValueOf(char.toString, Delimiters)
        case _ => false
      }
    }

    def addDelimToken(str: String): Unit = {
      Delimiters.getValue(str) match { case Some(delim) => addToken(Delimiter(delim, str)) }
    }

    if (isDelimiter && !nextIsDelimiter) {
      addDelimToken(curr.toString)
      advanceChar()
    } else if (isDelimiter && nextIsDelimiter) { // could be 2-char delim or 2 separate 1-char delims
      val twoCharDelim = curr.toString + source(index + 1).toString
      if (isValueOf(twoCharDelim, Delimiters)) {
        addDelimToken(twoCharDelim)
        advanceChar()
        advanceChar()
      } else {
        addDelimToken(curr.toString)
        advanceChar()
        addDelimToken(curr.toString)
        advanceChar()
      }
    } else {
      // throw error, not a valid delimiter combo
    }
  }

  @tailrec
  def handleNumValue(numVal: String = ""): Unit = {
    if (hasNext && isNumValue) {
      val tmpNumVal = numVal + curr.toString
      advanceChar()
      handleNumValue(tmpNumVal)
    } else {
      addToken(Value(numVal))
    }
  }

  @tailrec
  def handleTerm(term: String = ""): Unit = {
    if (hasNext && (curr.isLetterOrDigit || curr == '_')) {
      val tmpTerm = term + curr.toString
      advanceChar()
      handleTerm(tmpTerm)
    } else {
      if (isKeyword(term))
        Keywords.getValue(term) match { case Some(keywordValue) => addToken(Keyword(keywordValue, term)) }
      else if (isIdent(term)) {
        addToken(Ident(term))
      }
    }
  }

  @tailrec
  def scanHelper(): Unit = {
    if (hasNext) {
      if (handleQuotes()) {
        if (isValidCharacter) {
          if (isComment)
            skipLine()
          else if (isTerminator) {
            addToken(Terminator(curr.toString.replace("\n", "\\n")))
            newline(curr == ';')
          }
          else if (isWhitespace) advanceChar()
          else if (isRawDelimiter) handleDelimiter()
          else if (isNumValue) handleNumValue()
          else handleTerm()
        }
        else {
          println(s"Invalid character: $curr")
          advanceChar()
        }
        scanHelper()
      }
      else addToken(EOF())
    }
    else addToken(EOF())
  }

  def scan(sourceString: String): ArrayBuffer[Token] = {
    var tmpSourceString = sourceString.replace("\r", "")
    if (tmpSourceString.endsWith("\n"))
      tmpSourceString = tmpSourceString.dropRight(1)

    position.source = tmpSourceString
    position.lineList = position.source.split("\n")
    scanHelper()

    println("Tokens:")
    tokenStream.foreach(println(_))

    tokenStream
  }

  def clear(): Unit = {
    position.clear()
    tokenStream.clear()
  }
}