import org.scalatest.flatspec.AnyFlatSpec
import BantRunner.Main
import Lexer.{Lexer, SyntaxDefinitions}
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
    Lexer.scan("c c")
    assert(Lexer.tokenStream(0).tokenText == "c")
  }

  it should "return true if non-quoted char and last char" in {
    Lexer.clear()
    Lexer.scan("c")
    assert(Lexer.tokenStream(0).tokenText == "c")
  }

  it should "return false if quoted char and last char" in {
    Lexer.clear()
    Lexer.scan("\'c\'")
    assert(Lexer.tokenStream(0).tokenText == "\'c\'")
  }

  it should "return false if quoted string and last char" in {
    Lexer.clear()
    Lexer.scan("\"test\"")
    assert(Lexer.tokenStream(0).tokenText == "\"test\"")
  }

  it should "return false if quoted string and last char and space" in {
    Lexer.clear()
    Lexer.scan("\"test test2\"")
    assert(Lexer.tokenStream(0).tokenText == "\"test test2\"")
  }

  "Lexer.handleDelimiter" should "add to tokenStream if delim exists" in {
    Lexer.clear()
    Lexer.scan("+")
    assert(Lexer.tokenStream(0).tokenText == "+")
  }

  it should "add to tokenStream twice if 2 delims exists" in {
    Lexer.clear()
    Lexer.scan("+-")
    assert(Lexer.tokenStream(0).tokenText == "+" &&
      Lexer.tokenStream(1).tokenText == "-")
  }

  it should "add to tokenStream once if 2-char delim exists" in {
    Lexer.clear()
    Lexer.scan(":>")
    assert(Lexer.tokenStream.size == 2 &&
      Lexer.tokenStream(0).tokenText == ":>")
  }

  it should "not add to tokenStream if no delim exists" in {
    Lexer.clear()
    Lexer.scan("c")
    assert(Lexer.tokenStream.size == 2 &&
      Lexer.tokenStream(0).tokenText == "c")
  }

  "Lexer.handleNumValue" should "add to tokenStream numeric value exists" in {
    Lexer.clear()
    Lexer.scan("12345")
    assert(Lexer.tokenStream.size == 2 &&
      Lexer.tokenStream(0).tokenText == "12345")
  }

  it should "not add to tokenStream if no numeric value exists" in {
    Lexer.clear()
    Lexer.scan("test")
    assert(Lexer.tokenStream.size == 2 &&
      Lexer.tokenStream(0).tokenText == "test")
  }

  "Lexer.handleTerm" should "add Keyword or Ident to tokenStream if curr is one" in {
    Lexer.clear()
    Lexer.scan("val")
    assert(Lexer.tokenStream.size == 2 &&
      Lexer.tokenStream(0).tokenText == "val")

    Lexer.clear()
    Lexer.scan("test")
    assert(Lexer.tokenStream.size == 2 &&
      Lexer.tokenStream(0).tokenText == "test")
  }

  "Lexer.scan and Lexer.scanHelper" should "make entire tokenStream" in {
    Lexer.clear()
    Lexer.scan("val residences: List[Residence] = List { House(3, 2), Apartment(1, 1), Condo(2, 1) }\n\nforeach[Residence](residences, (r: Residence) => r.printDimensions())\n\nval house = House(3, 2)\n\nval optHouse: Option[House] = house match {\n    case House(bed, bath) => Some(House(bed, bath))\n    case _ => None\n}")
    assert(Lexer.tokenStream.size == 102)
  }

  "Lexer.clear" should "clear all Lexer & Position vars" in {
    Lexer.clear()
    Lexer.scan("test\ntest2")

    Lexer.clear()

    assert(Lexer.position.index == 0 &&
      Lexer.position.lineNumber == 0 &&
      Lexer.position.columnNumber == 0 &&
      Lexer.position.lineText == "" &&
      !Lexer.position.inQuotes &&
      Lexer.position.source == "" &&
      Lexer.position.lineList.isEmpty &&
      Lexer.tokenStream.isEmpty)
  }

  "Position.hasNext" should "return false if curr is not last value" in {
    Lexer.clear()
    Lexer.position.source = "cd"
    assert(Lexer.position.hasNext)
  }

  it should "return true if curr is last value" in {
    Lexer.clear()
    Lexer.position.source = "c"
    assert(Lexer.position.hasNext)
  }

  "Position.curr" should "give the character at the index of the source string" in {
    Lexer.clear()
    Lexer.position.source = "test"
    Lexer.position.index = 1
    assert(Lexer.position.curr == 'e')
  }

  "Position.next" should "give the next character to the index of the source string" in {
    Lexer.clear()
    Lexer.position.source = "test"
    Lexer.position.index = 1
    assert(Lexer.position.next == 's')
  }

  "Position.peek" should "return true if char is not at end of source" in {
    Lexer.clear()
    Lexer.position.index = 2
    Lexer.position.source = "test"
    assert(Lexer.position.peek() match {
      case Some(character) => character == 't'
      case None => false
    })
  }

  it should "return false if char is at end of source" in {
    Lexer.clear()
    Lexer.position.index = 3
    Lexer.position.source = "test"
    assert(!(Lexer.position.peek() match {
      case Some(_) => true
      case None => false
    }))
  }

  "Position.advanceChar" should "increment column, index, and append to lineText if space char" in {
    Lexer.clear()
    Lexer.position.index = 4
    Lexer.position.columnNumber = 4
    Lexer.position.lineText = "test"
    Lexer.position.source = "test test2"

    Lexer.position.advanceChar()

    assert(Lexer.position.index == 5 &&
      Lexer.position.columnNumber == 5 &&
      Lexer.position.lineText == "test ")
  }

  it should "increment column, index, and append to lineText if tab char" in {
    Lexer.clear()
    Lexer.position.index = 4
    Lexer.position.columnNumber = 4
    Lexer.position.lineText = "test"
    Lexer.position.source = "test\ttest2"

    Lexer.position.advanceChar()

    assert(Lexer.position.index == 8 &&
      Lexer.position.columnNumber == 8 &&
      Lexer.position.lineText == "test\t")
  }

  it should "increment column, index, and append to lineText if regular char" in {
    Lexer.clear()
    Lexer.position.index = 2
    Lexer.position.columnNumber = 2
    Lexer.position.lineText = "te"
    Lexer.position.source = "test test2"

    Lexer.position.advanceChar()

    assert(Lexer.position.index == 3 &&
      Lexer.position.columnNumber == 3 &&
      Lexer.position.lineText == "tes")
  }

  "Position.resetLine" should "reset the Position vars for a new line" in {
    Lexer.clear()
    Lexer.position.lineNumber = 2
    Lexer.position.columnNumber = 4
    Lexer.position.lineText = "test"

    Lexer.position.resetLine()

    assert(Lexer.position.lineNumber == 3 &&
      Lexer.position.columnNumber == 0 &&
      Lexer.position.lineText == "")
  }

  "Position.newLine" should "reset the Position vars on newline char, index += 1" in {
    Lexer.clear()
    Lexer.position.index = 4
    Lexer.position.lineNumber = 2
    Lexer.position.columnNumber = 4
    Lexer.position.lineText = "test\ntest2"
    Lexer.position.source = "test\ntest2"

    println(Lexer.position.curr)

    Lexer.position.newline()

    assert(Lexer.position.index == 5 &&
      Lexer.position.lineNumber == 3 &&
      Lexer.position.columnNumber == 0 &&
      Lexer.position.lineText == "")
  }

  it should "reset the Position vars on semicolon char, index += 2" in {
    Lexer.clear()
    Lexer.position.index = 4
    Lexer.position.lineNumber = 2
    Lexer.position.columnNumber = 4
    Lexer.position.lineText = "test;\ntest2"
    Lexer.position.source = "test;\ntest2"

    println(Lexer.position.curr)

    Lexer.position.newline(true)

    assert(Lexer.position.index == 6 &&
      Lexer.position.lineNumber == 3 &&
      Lexer.position.columnNumber == 0 &&
      Lexer.position.lineText == "")
  }

  "Position.filePositionFactory" should "create a FilePosition instance" in {
    Lexer.clear()
    Lexer.position.lineNumber = 1
    Lexer.position.columnNumber = 5
    Lexer.position.lineList = Array("test", "test2")
    val fp = Lexer.position.filePositionFactory
    assert(fp.line == 1 && fp.column == 5 && fp.lineText == "test2")
  }

  "Position.clear" should "clear all Position vars" in {
    Lexer.clear()
    Lexer.position.index = 5
    Lexer.position.lineNumber = 5
    Lexer.position.columnNumber = 5
    Lexer.position.lineText = "test"
    Lexer.position.inQuotes = true
    Lexer.position.source = "test\ntest2"
    Lexer.position.lineList = Array("test", "test2")

    Lexer.position.clear()

    assert(Lexer.position.index == 0 &&
          Lexer.position.lineNumber == 0 &&
          Lexer.position.columnNumber == 0 &&
          Lexer.position.lineText == "" &&
          !Lexer.position.inQuotes &&
          Lexer.position.source == "" &&
          Lexer.position.lineList.isEmpty)
  }
}
