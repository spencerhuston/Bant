package Parser

import Lexer.SyntaxDefinitions.Keywords
import Lexer.SyntaxDefinitions.Keywords._
import Lexer.{EOF, Keyword, Token}

import scala.collection.mutable.ArrayBuffer

object Parser {
  var index = 0
  var tokenStream: ArrayBuffer[Token] = ArrayBuffer[Token]()

  def curr: Token = tokenStream(index)
  def isEof: Boolean = {
    index == tokenStream.length - 1 && (
      curr match {
        case EOF(_, _) => true
        case _ => false
      })
  }

  def parse(tokens: ArrayBuffer[Token]): Unit = {
    tokenStream = tokens
    parseExp()
  }

  def parseExp(): Unit = {
    if (!isEof) {
      curr match {
        case Keyword(VAL, tokenText, fp) => parseLet()
        case Keyword(INCLUDE, tokenText, fp) => parseInclude()
        case _ => parseSimpleExp()
      }
    }
  }

  def parseLet(): Unit = {

  }

  def parseInclude(): Unit = {

  }

  def parseSimpleExp(): Unit = {
    if (!isEof) {
      curr match {
        case Keyword(IF, tokenText, fp) => parseBranch()
        case Keyword(LIST, tokenText, fp) => parseCollectionValue(LIST)
        case Keyword(TUPLE, tokenText, fp) => parseCollectionValue(TUPLE)
        case Keyword(SET, tokenText, fp) => parseCollectionValue(SET)
        case Keyword(DICT, tokenText, fp) => parseCollectionValue(DICT)
        case Keyword(MATCH, tokenText, fp) => parseMatch()
        case Keyword(SWITCH, tokenText, fp) => parseSwitch()
        case Keyword(TYPECLASS, tokenText, fp) => parseTypeclass()
        case Keyword(INSTANCE, tokenText, fp) => parseInstance()
        case Keyword(TYPE, tokenText, fp) => parseAdt()
        case Keyword(FN, tokenText, fp) => parseProg()
        case _ => parseUtight()
      }
    }
  }

  def parseBranch(): Unit = {

  }

  def parseCollectionValue(keyword: Keywords.Value): Unit = {

  }

  def parseMatch(): Unit = {

  }

  def parseSwitch(): Unit = {

  }

  def parseTypeclass(): Unit = {

  }

  def parseInstance(): Unit = {

  }

  def parseAdt(): Unit = {

  }

  def parseProg(): Unit = {

  }

  def parseUtight(): Unit = {

  }
}
