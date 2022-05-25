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
    tokenText = ""
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
                                  curr == '#'

  def isComment: Boolean = !inQuotes && curr == '#'

  def isTerminator: Boolean = curr == ';' || curr == '\n'

  def isWhitespace: Boolean = curr == ' ' || curr == '\t'

  def isRawDelimiter: Boolean = isValueOf(curr.toString, RawDelimiters)

  def isDelimiter: Boolean = isValueOf(curr.toString, Delimiters) || isValueOf(curr.toString, Operators)

  //def isKeyword: Boolean = Keywords.values.toList.contains(???) // WRONG IMPLEMENTATION

  def isNumValue: Boolean = curr.isDigit

  //def isIdent: Boolean = ???

  def handleQuotes(): Unit = {
    if (curr == '\'' || curr == '\"') {
      inQuotes = !inQuotes
      advanceChar()
      tokenStream += Value(tokenText)
    } else if (inQuotes) {
      tokenText += curr
      advanceChar()
    }
  }

  def handleDelimiter(): Unit = {
    def nextIsDelimiter: Boolean = {
      peek() match {
        case Some(char) => isValueOf(char.toString, Delimiters) || isValueOf(char.toString, Operators)
        case _ => false
      }
    }

    if (isDelimiter && !nextIsDelimiter) {
      tokenStream += Delimiter(curr.toString)
      advanceChar()
    } else if (isDelimiter && nextIsDelimiter) { // could be 2-char delim or 2 separate 1-char delims
      val twoCharDelim = curr.toString + peek().toString
      if (isValueOf(twoCharDelim, Delimiters) || isValueOf(twoCharDelim, Operators)) {
        tokenStream += Delimiter(twoCharDelim)
        advanceChar()
        advanceChar()
      } else {
        tokenStream += Delimiter(curr.toString)
        advanceChar()
        tokenStream += Delimiter(curr.toString)
        advanceChar()
      }
    } else {
      // throw error, not a valid delimiter combo
    }
  }

  @tailrec
  def handleNumValue(numVal: String = ""): String = {
    if (isNumValue) {
      val tmpNumVal = numVal + curr.toString
      advanceChar()
      handleNumValue(tmpNumVal)
    } else {
      numVal
    }
  }

  def handleKeyword(): Unit = {

  }

  @tailrec
  def scanHelper(): Unit = {
    if (hasNext) {
      handleQuotes()
      if (isValidCharacter) {
        if (isComment)
          skipLine()
        else if (isTerminator) {
          addToken(Terminator(curr.toString))
          newline(curr == ';')
        }
        else if (isWhitespace) advanceChar()
        else { // is actual delimiter, keyword, value, or ident
          if (isRawDelimiter) handleDelimiter()
          else if (isNumValue) addToken(Value(handleNumValue()))
          /* else if (isKeyword) {
            ???
          }
          else if (isIdent) {
            ???
          }*/
        }
      }
      else {
        //println(s"Invalid character: $curr")
      }

      scanHelper()
    }
    else
      addToken(EOF())
  }


  def scan(sourceString: String): Unit = {
    position.source = sourceString
    scanHelper()
    println("Tokens:")
    tokenStream.foreach(println(_))
  }
}