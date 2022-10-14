package Lexer

import Logger.Level
import Logger.Logger.{ERROR, LOG, LOG_HEADER, WARN, lineList}
import Position._
import SyntaxDefinitions._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Lexer {
  var position: Position.type = Position
  var tokenStream: ArrayBuffer[Token] = ArrayBuffer[Token]()
  var errorOccurred = false

  def addToken[T <: Token](token: T): Unit = {
    tokenStream += token
    LOG(Level.DEBUG, s"Token added: $token")
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

  def isNewline: Boolean = curr == '\n'

  def isSemicolon: Boolean = curr == ';'

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

  def findClosestKeyword(str: String): Unit = {
    if (str.length == 1)
      return

    var minCharacterOffCount = 100
    var minCharacterOffCountKeyword = ""
    var minCompareDiff = 100
    var minCompareDiffKeyword = ""
    var minCharacterOffCountReverse = 100
    var minCharacterOffCountReverseKeyword = ""
    var minCompareDiffReverse = 100
    var minCompareDiffReverseKeyword = ""

    Keywords.values.foreach((keywordVal: Keywords.Value) => {
      val keywordStr = keywordVal.toString
      var characterOffCount = Math.abs(keywordStr.length - str.length)

      for (i <- 0 until (if (keywordStr.length < str.length) keywordStr else str).length) {
        characterOffCount += (if (keywordStr(i) != str(i)) 1 else 0)
      }

      var characterOffCountReverse = Math.abs(keywordStr.length - str.length)
      for (i <- 0 until (if (keywordStr.length < str.length) keywordStr else str).length) {
        characterOffCountReverse += (if (keywordStr.reverse(i) != str.reverse(i)) 1 else 0)
      }

      val compareDiff = keywordStr.compareTo(str)
      val compareDiffReverse = keywordStr.reverse.compareTo(str.reverse)

      if (characterOffCount < minCharacterOffCount) {
        minCharacterOffCount = characterOffCount
        minCharacterOffCountKeyword = keywordStr
      }
      if (compareDiff < minCompareDiff) {
        minCompareDiff = compareDiff
        minCompareDiffKeyword = keywordStr
      }
      if (characterOffCountReverse < minCharacterOffCountReverse) {
        minCharacterOffCountReverse = characterOffCountReverse
        minCharacterOffCountReverseKeyword = keywordStr
      }
      if (compareDiffReverse < minCompareDiffReverse) {
        minCompareDiffReverse = compareDiffReverse
        minCompareDiffReverseKeyword = keywordStr
      }
    })

    var closestKeyword = minCharacterOffCountKeyword
    var closestValue = minCharacterOffCount
    if (Math.abs(minCompareDiff) < Math.abs(minCharacterOffCount)) {
      closestKeyword = minCompareDiffKeyword
      closestValue = Math.abs(minCompareDiff)
    }
    if (Math.abs(minCharacterOffCountReverse) < Math.abs(closestValue)) {
      closestKeyword = minCharacterOffCountReverseKeyword
      closestValue = Math.abs(minCharacterOffCountReverse)
    }
    if (Math.abs(minCompareDiffReverse) < Math.abs(closestValue)) {
      closestKeyword = minCompareDiffReverseKeyword
      closestValue = Math.abs(minCompareDiffReverse)
    }

    val score = Math.sqrt(Math.abs(minCharacterOffCount) *
      Math.abs(minCompareDiff) *
      Math.abs(minCharacterOffCountReverse) *
      Math.abs(minCompareDiffReverse)) /
      (minCharacterOffCount +
        minCompareDiff +
        minCharacterOffCountReverse +
        minCompareDiffReverse)

    if (Math.abs(score) < 0.5 ||
      minCharacterOffCount == 1 ||
      minCharacterOffCountReverse == 1) {
      //println(s"$str: $score, $minCharacterOffCount, $minCharacterOffCountReverse")
      warnIdentForKeyword(s"Warning: $str: Did you mean $closestKeyword?", str)
    }
  }

  @tailrec
  def handleQuotes(str: String = ""): Boolean = {
    if (hasNext) {
      if (!inQuotes && curr == '\"') {
        inQuotes = true
        val tmpStr = str + curr.toString
        advanceChar()
        handleQuotes(tmpStr)
      } else if (inQuotes && curr != '\"') {
        val tmpStr = str + curr.toString
        advanceChar()
        handleQuotes(tmpStr)
      } else if (inQuotes && curr == '\"') {
        addToken(Value(str + curr.toString))
        inQuotes = false
        advanceChar()

        hasNext
      } else if (!inQuotes && curr == '\'' &&
        position.index <= position.source.length - 3) {
        addToken(Value("\'" + source(index + 1).toString + "\'"))
        advanceChar()
        advanceChar()
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
      reportInvalidCharacter()
      advanceChar()
    }
  }

  @tailrec
  def handleNumValue(numVal: String = ""): Unit = {
    if (hasNext && isNumValue) {
      val tmpNumVal = numVal + curr.toString
      advanceChar()
      handleNumValue(tmpNumVal)
    } else {
      val actualColumnNumber = position.columnNumber
      position.columnNumber = actualColumnNumber - numVal.length
      addToken(Value(numVal))
      position.columnNumber = actualColumnNumber
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
        Keywords.getValue(term) match {
          case Some(keywordValue) =>
            val actualColumnNumber = position.columnNumber
            position.columnNumber = actualColumnNumber - term.length
            addToken(Keyword(keywordValue, term))
            position.columnNumber = actualColumnNumber
        }
      else if (isIdent(term)) {
        findClosestKeyword(term)

        val actualColumnNumber = position.columnNumber
        position.columnNumber = actualColumnNumber - term.length
        addToken(Ident(term))
        position.columnNumber = actualColumnNumber
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
          else if (isNewline)
            newline()
          else if (isSemicolon) {
            addToken(Terminator(";"))
            advanceChar()
          }
          else if (isWhitespace) advanceChar()
          else if (isRawDelimiter) handleDelimiter()
          else if (isNumValue) handleNumValue()
          else handleTerm()
        }
        else {
          reportInvalidCharacter()
          advanceChar()
        }
        scanHelper()
      }
      else {
        addToken(EOF())
      }
    }
    else {
      addToken(EOF())
    }
  }

  @tailrec
  def stripMultiSemicolons(tokenStream: ArrayBuffer[Token], index: Int = 0, semicolonIndex: Int = -1): ArrayBuffer[Token] = {
    tokenStream(index) match {
      case Terminator(_, _) =>
        if (semicolonIndex == -1)
          stripMultiSemicolons(tokenStream, index + 1, index)
        else
          stripMultiSemicolons(tokenStream, index + 1, semicolonIndex)
      case EOF(_, _) =>
        if (semicolonIndex != -1) {
          tokenStream.remove(semicolonIndex, index - semicolonIndex - 1)
        }
        tokenStream
      case _ =>
        if (semicolonIndex != -1) {
          tokenStream.remove(semicolonIndex, index - semicolonIndex - 1)
          stripMultiSemicolons(tokenStream, semicolonIndex + 1)
        }
        else
          stripMultiSemicolons(tokenStream, index + 1)
    }
  }

  def scan(sourceString: String): ArrayBuffer[Token] = {
    var tmpSourceString = sourceString.replace("\r", "")
    if (tmpSourceString.endsWith("\n"))
      tmpSourceString = tmpSourceString.dropRight(1)

    position.source = tmpSourceString
    lineList = position.source.split("\n")
    scanHelper()

    tokenStream = stripMultiSemicolons(tokenStream)
    var tokenStreamStringList = ""
    tokenStream.foreach(tokenStreamStringList += _ + "\n")
    LOG_HEADER("TOKENS", tokenStreamStringList)

    tokenStream
  }

  def reportInvalidCharacter(): Unit = {
    ERROR(s"Error: Invalid character: $curr")
    ERROR(s"Line: ${position.lineNumber + 1}, Column: ${position.columnNumber + 1}:\n")
    ERROR(s"${lineList(position.lineNumber)}")
    ERROR(s"${" " * position.columnNumber}^\n")
    errorOccurred = true
  }

  def warnIdentForKeyword(str: String, ident: String): Unit = {
    WARN(s"$str")
    WARN(s"Line: ${position.lineNumber + 1}, Column: ${position.columnNumber - ident.length + 1}:\n")
    WARN(s"${lineList(position.lineNumber)}")
    WARN(s"${" " * (position.columnNumber - ident.length)}^\n")
  }

  def clear(): Unit = {
    position.clear()
    tokenStream.clear()
  }
}