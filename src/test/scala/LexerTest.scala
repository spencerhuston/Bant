import org.scalatest.flatspec.AnyFlatSpec

import BantRunner.Main
import Lexer.{Lexer, Position, SyntaxDefinitions, Token}
import SyntaxDefinitions.{Delimiters, Keywords, RawDelimiters}

class LexerTest extends AnyFlatSpec {
  "Lexer.addToken" should "add a token to the stream" in {
    Main.clear()
    Lexer.scan("test\n")
    assert(Lexer.tokenStream.size == 2)
  }

  "Lexer.isValueOf" should "return true if value is member of its enum" in {
    assert(Lexer.isValueOf("+", RawDelimiters) &&
      Lexer.isValueOf("+", Delimiters) &&
      Lexer.isValueOf("true", Keywords))
  }

  "Lexer.isValidCharacter" should "return true for _, num, char, delims" in {
    Lexer.clear()
    Lexer.position.source = "_"
    assert(Lexer.isValidCharacter)

    Lexer.position.source = "c"
    assert(Lexer.isValidCharacter)

    Lexer.position.source = "0"
    assert(Lexer.isValidCharacter)

    Lexer.position.source = "#"
    assert(Lexer.isValidCharacter)

    Lexer.position.source = ">"
    assert(Lexer.isValidCharacter)
  }

  it should "return false for invalid char" in {
    Lexer.position.source = "$"
    assert(!Lexer.isValidCharacter)
  }

  "Lexer.isComment" should "return true if line starts with #" in {
    Lexer.clear()
    Lexer.position.source = "#test"
    assert(Lexer.isComment)
  }

  it should "return false if line doesn't start with #" in {
    Lexer.clear()
    Lexer.position.source = "test"
    assert(!Lexer.isComment)
  }

  "Lexer.isTerminator" should "return true if curr is terminator" in {
    Lexer.clear()
    Lexer.position.source = ";"
    assert(Lexer.isTerminator)

    Lexer.position.source = "\n"
    assert(Lexer.isTerminator)
  }

  it should "return false if curr is not terminator" in {
    Lexer.clear()
    Lexer.position.source = "c"
    assert(!Lexer.isTerminator)
  }

  "Lexer.isWhitespace" should "return true if curr is whitespace" in {
    Lexer.clear()
    Lexer.position.source = " "
    assert(Lexer.isWhitespace)

    Lexer.position.source = "\t"
    assert(Lexer.isWhitespace)
  }

  it should "return false if curr is not whitespace" in {
    Lexer.clear()
    Lexer.position.source = "c"
    assert(!Lexer.isWhitespace)
  }

  "Lexer.isRawDelimiter" should "return true if curr is a raw delim" in {
    Lexer.clear()
    Lexer.position.source = ":"
    assert(Lexer.isRawDelimiter)
  }

  it should "return false if curr is not a raw delim" in {
    Lexer.clear()
    Lexer.position.source = "c"
    assert(!Lexer.isRawDelimiter)
  }

  "Lexer.isDelimiter" should "return true if curr is a delim" in {
    Lexer.clear()
    Lexer.position.source = ":>"
    assert(Lexer.isDelimiter)
  }

  it should "return false if curr is not a delim" in {
    Lexer.clear()
    Lexer.position.source = "c"
    assert(!Lexer.isDelimiter)
  }

  "Lexer.isNumValue" should "return true if curr is a value" in {
    Lexer.clear()
    Lexer.position.source = "5627"
    assert(Lexer.isNumValue)
  }

  it should "return false if curr is not a value" in {
    Lexer.clear()
    Lexer.position.source = "c"
    assert(!Lexer.isNumValue)
  }

  "Lexer.isKeyword" should "return true if curr is a keyword" in {
    assert(Lexer.isKeyword("List"))
  }

  it should "return false if curr is not a keyword" in {
    assert(!Lexer.isKeyword("c"))
  }

  "Lexer.isIdent" should "return true if curr follows ident regex" in {
    assert(Lexer.isIdent("testIdent") &&
      Lexer.isIdent("_underscoreIdent") &&
      !Lexer.isIdent("5startwithNumberIdent") &&
      Lexer.isIdent("numberPresentIdent1423421") &&
      Lexer.isIdent("underscored_mixed_in_") &&
      !Lexer.isIdent("!$noInvalidCharacters$"))
  }

  "Lexer.handleQuotes" should "return true if non-quoted char and not last char" in {
    Lexer.clear()
    Lexer.position.source = "c c"
    assert(Lexer.handleQuotes())
  }

  it should "return true if non-quoted char and last char" in {
    Lexer.clear()
    Lexer.position.source = "c"
    assert(Lexer.handleQuotes())
  }

  it should "return false if quoted char and last char" in {
    Lexer.clear()
    Lexer.position.source = "\'c\'"
    assert(!Lexer.handleQuotes())
  }

  it should "return false if quoted string and last char" in {
    Lexer.clear()
    Lexer.position.source = "\"test\""
    assert(!Lexer.handleQuotes())
  }

  "Lexer.handleDelimiter" should "add to tokenStream if delim exists" in {
    Lexer.clear()
    Lexer.position.source = "+"
    Lexer.handleDelimiter()
    assert(Lexer.tokenStream.size == 1)
  }

  it should "add to tokenStream twice if 2 delims exists" in {
    Lexer.clear()
    Lexer.position.source = "+-"
    Lexer.handleDelimiter()
    assert(Lexer.tokenStream.size == 2)
  }

  it should "add to tokenStream once if 2-char delim exists" in {
    Lexer.clear()
    Lexer.position.source = ":>"
    Lexer.handleDelimiter()
    assert(Lexer.tokenStream.size == 1)
  }

  it should "not add to tokenStream if no delim exists" in {
    Lexer.clear()
    Lexer.position.source = "c"
    Lexer.handleDelimiter()
    assert(Lexer.tokenStream.isEmpty)
  }

  //"Lexer.handleNumValue"
  //"Lexer.handleTerm"
  //"Lexer.scanHelper"
  //"Lexer.scan"
  //"Lexer.clear"

  //"Position.hasNext"
  //"Position.curr"
  //"Position.next"
  //"Position.peek"
  //"Position.advanceChar"
  //"Position.resetLine"
  //"Position.newLine"
  //"Position.filePositionFactory"
  //"Position.clear"
}
