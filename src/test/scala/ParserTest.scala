import org.scalatest.flatspec.AnyFlatSpec
import Lexer.SyntaxDefinitions.Delimiters._
import Lexer.SyntaxDefinitions.Keywords._
import Lexer.{Delimiter, EOF, FilePosition, Ident, Keyword, Token}
import Logger.Logger.lineList
import Parser.{BoolVal, Branch, IntVal, Let, Lit, NoOp, NullVal, Ref}
import Parser.Parser._
import TypeChecker.{IntType, UnknownType}

import scala.collection.mutable.ArrayBuffer

class ParserTest extends AnyFlatSpec {
  def clear(): Unit = {
    index = 0
    tokenStream = ArrayBuffer[Token]()
    errorOccurred = false
    dummyCount = 0
    anonCount = 0
    lineList = Array[String]()
    BantRunner.Main.clear()
  }

  def fp: FilePosition = FilePosition(0, 0, "val")

  "Parser.curr" must "give the current token" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp))
    index = 0
    assert(curr.tokenText == "val")
  }

  it should "fail if out of bounds" in {
    index = 1
    assertThrows[IndexOutOfBoundsException] {
      curr.tokenText == "val"
    }
  }

  "Parser.advance" must "advance the index" in {
    clear()
    index = 0
    advance()
    assert(index == 1)
  }

  "Parser.none" must "return a no-op expression" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp))
    index = 0
    assert(none.token.tokenText == "val")
  }

  "Parser.isEof" must "return true if curr is EOF" in {
    clear()
    tokenStream = ArrayBuffer[Token](EOF("EOF", fp))
    index = 0
    assert(isEof)
  }

  it should "return false if curr is not EOF" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp))
    index = 0
    assert(!isEof)
  }

  "Parser.matchRequired" must "return true if match is made and advance" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp))
    index = 0
    assert(matchRequired(VAL) && index == 1)
  }

  it should "return false if match is not made" in {
    clear()
    lineList = Array[String]("val")
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", FilePosition(0, 0, "val")))
    index = 0
    assert(!matchRequired(LAZY) && index == 1 && errorOccurred)
  }

  "Parser.matchOptional" must "return true if match is made and advance" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp))
    index = 0
    assert(matchOptional(VAL) && index == 1)
  }

  it should "return false if match is not made and not advance" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", FilePosition(0, 0, "val")))
    index = 0
    assert(!matchOptional(LAZY) && index == 0 && !errorOccurred)
  }

  "Parser.peek" should "return true if next value matches" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp), Keyword(LAZY, "lazy", fp))
    index = 0
    assert(peek(LAZY))
  }

  it should "return false if next value does not match" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp), Keyword(LAZY, "lazy", fp))
    index = 0
    assert(!peek(VAL))
  }

  it should "return false curr is last token" in {
    clear()
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp))
    index = 0
    assert(!peek(LAZY))
  }

  "Parser.matchIdent" should "return ident if curr is one" in {
    clear()
    tokenStream = ArrayBuffer[Token](Ident("name", fp))
    index = 0
    assert(matchIdent == "name" && index == 1)
  }

  it should "return empty string if curr is not ident" in {
    clear()
    lineList = Array[String]("val")
    tokenStream = ArrayBuffer[Token](Keyword(VAL, "val", fp))
    index = 0
    assert(matchIdent != "name" && index == 0)
  }

  "Parser.isBinaryOperator" must "return true if op is arith or boolean binary op" in {
    clear()
    tokenStream = ArrayBuffer[Token](Delimiter(PLUS, "+", fp))
    index = 0
    assert(isBinaryOperator(0))
  }

  it must "return false if op is not arith or boolean binary op" in {
    clear()
    tokenStream = ArrayBuffer[Token](Delimiter(LAMBDA, "|", fp))
    index = 0
    assert(!isBinaryOperator(0))
  }

  it must "return false if op min is less than curr op precedence" in {
    clear()
    tokenStream = ArrayBuffer[Token](Delimiter(PLUS, "+", fp))
    index = 0
    assert(!isBinaryOperator(5))
  }

  "Parser.getPrecedence" must "return corresponding value for each op" in {
    clear()
    assert(getPrecedence(AND) == 0 && getPrecedence(OR) == 0)
    assert(getPrecedence(PLUS) == 2 && getPrecedence(MINUS) == 2)
    assert(getPrecedence(MULTIPLY) == 3 &&
      getPrecedence(DIVIDE) == 3 &&
      getPrecedence(MODULUS) == 3)
    assert(getPrecedence(GREATER_THAN) == 1)
  }

  "Parser.parseExp" must "return simplest let when given tokens" in {
    clear()
    val path = "src/test/testPrograms/ParserPrograms/let_test.bnt"
    val ts = BantRunner.Main.getSource(Array[String]("-f", path)) match {
      case Some(source) =>
        Lexer.Lexer.scan(source)
    }

    val exp = parse(ts)
    assert(exp.isInstanceOf[Let])
    val let = exp.asInstanceOf[Let]
    assert(!let.isLazy)
    assert(let.ident == "a")
    assert(let.letType == UnknownType())
    assert(let.expType == UnknownType())
    assert(let.expValue.isInstanceOf[Lit])
    assert(let.expValue.asInstanceOf[Lit].value == IntVal(0))
    assert(let.afterLet.isInstanceOf[NoOp])
  }

  it must "return let with annotations" in {
    clear()
    val path = "src/test/testPrograms/ParserPrograms/let_test2.bnt"
    val ts = BantRunner.Main.getSource(Array[String]("-f", path)) match {
      case Some(source) =>
        Lexer.Lexer.scan(source)
    }

    val exp = parse(ts)
    assert(exp.isInstanceOf[Let])
    val let = exp.asInstanceOf[Let]
    assert(let.isLazy)
    assert(let.ident == "a")
    assert(let.letType == IntType())
    assert(let.expType == UnknownType())
    assert(let.expValue.isInstanceOf[Lit])
    assert(let.expValue.asInstanceOf[Lit].value == IntVal(0))
    assert(let.afterLet.isInstanceOf[Ref])
  }

  it must "return lit" in {
    clear()
    val path = "src/test/testPrograms/ParserPrograms/lit_test.bnt"
    val ts = BantRunner.Main.getSource(Array[String]("-f", path)) match {
      case Some(source) =>
        Lexer.Lexer.scan(source)
    }

    val exp = parse(ts)
    assert(exp.isInstanceOf[Lit])
    val lit = exp.asInstanceOf[Lit]
    assert(lit.value == IntVal(5))
  }

  "Parser.parseLet" must "return simplest let when given tokens" in {
    clear()
    val path = "src/test/testPrograms/ParserPrograms/let_test.bnt"
    val ts = BantRunner.Main.getSource(Array[String]("-f", path)) match {
      case Some(source) =>
        Lexer.Lexer.scan(source)
    }

    val exp = parse(ts)
    assert(exp.isInstanceOf[Let])
    val let = exp.asInstanceOf[Let]
    assert(!let.isLazy)
    assert(let.ident == "a")
    assert(let.letType == UnknownType())
    assert(let.expType == UnknownType())
    assert(let.expValue.isInstanceOf[Lit])
    assert(let.expValue.asInstanceOf[Lit].value == IntVal(0))
    assert(let.afterLet.isInstanceOf[NoOp])
  }

  it must "return let with annotations" in {
    clear()
    val path = "src/test/testPrograms/ParserPrograms/let_test2.bnt"
    val ts = BantRunner.Main.getSource(Array[String]("-f", path)) match {
      case Some(source) =>
        Lexer.Lexer.scan(source)
    }

    val exp = parse(ts)
    assert(exp.isInstanceOf[Let])
    val let = exp.asInstanceOf[Let]
    assert(let.isLazy)
    assert(let.ident == "a")
    assert(let.letType == IntType())
    assert(let.expType == UnknownType())
    assert(let.expValue.isInstanceOf[Lit])
    assert(let.expValue.asInstanceOf[Lit].value == IntVal(0))
    assert(let.afterLet.isInstanceOf[Ref])
  }

  "Parser.parseBranch" must "return branch exp" in {
    clear()
    val path = "src/test/testPrograms/ParserPrograms/branch_test.bnt"
    val ts = BantRunner.Main.getSource(Array[String]("-f", path)) match {
      case Some(source) =>
        Lexer.Lexer.scan(source)
    }

    val exp = parse(ts)
    assert(exp.isInstanceOf[Branch])
    val branch = exp.asInstanceOf[Branch]
    print(branch)
    assert(branch.condition.isInstanceOf[Lit] &&
      branch.condition.asInstanceOf[Lit].value == BoolVal(true))
    assert(branch.ifBranch.isInstanceOf[Lit] &&
      branch.ifBranch.asInstanceOf[Lit].value == IntVal(5))
    assert(branch.elseBranch.isInstanceOf[Lit] &&
      branch.elseBranch.asInstanceOf[Lit].value == IntVal(4))
  }

  it must "return branch exp with nullval elseBranch if else dne" in {
    clear()
    val path = "src/test/testPrograms/ParserPrograms/branch_test2.bnt"
    val ts = BantRunner.Main.getSource(Array[String]("-f", path)) match {
      case Some(source) =>
        Lexer.Lexer.scan(source)
    }

    val exp = parse(ts)
    assert(exp.isInstanceOf[Branch])
    val branch = exp.asInstanceOf[Branch]
    print(branch)
    assert(branch.condition.isInstanceOf[Lit] &&
      branch.condition.asInstanceOf[Lit].value == BoolVal(true))
    assert(branch.ifBranch.isInstanceOf[Lit] &&
      branch.ifBranch.asInstanceOf[Lit].value == IntVal(5))
    assert(branch.elseBranch.isInstanceOf[Lit] &&
      branch.elseBranch.asInstanceOf[Lit].value == NullVal())
  }

  "Parser.dummy" must "return dummy name" in {
    clear()
    assert(dummy == "dummy$0")
  }

  "Parser.anon" must "return anon name" in {
    clear()
    assert(anon == "anon$0")
  }
}