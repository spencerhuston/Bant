package Parser

import Lexer.SyntaxDefinitions.Delimiters._
import Lexer.SyntaxDefinitions.Delimiters
import Lexer.SyntaxDefinitions.Keywords._
import Lexer.{Delimiter, EOF, Ident, Keyword, Terminator, Token, Value}
import Logger.Level.INFO
import Logger.Logger.{ERROR, LOG, lineList}
import TypeChecker._

import scala.collection.mutable.ArrayBuffer

object Parser {
  var index = 0
  var tokenStream: ArrayBuffer[Token] = ArrayBuffer[Token]()
  var errorOccurred = false

  def curr: Token = tokenStream(index)
  def advance(): Unit = index += 1
  def none: NoOp = NoOp(curr)

  def isEof: Boolean = {
    index == tokenStream.length - 1 &&
      (curr match {
        case EOF(_, _) => true
        case _ => false
      })
  }

  def matchRequired[T <: Enumeration#Value](value: T): Boolean = {
    val matched = curr match {
      case Keyword(keywordValue, _, _) =>
        keywordValue == value
      case Delimiter(delimValue, _, _) =>
        delimValue == value
      case _ =>
        false
    }

    if (!matched)
      reportBadMatch(value.toString)

    advance()
    matched
  }

  def matchOptional[T <: Enumeration#Value](value: T): Boolean = {
    val matched = curr match {
      case Keyword(keywordValue, _, _) =>
        keywordValue == value
      case Delimiter(delimValue, _, _) =>
        delimValue == value
      case _ => false
    }

    if (matched)
      advance()

    matched
  }

  def matchTerminator: Boolean = {
    curr match {
      case Terminator(_, _) => true
      case _ => false
    }
  }

  def matchOperatorOptional: Boolean = {
    val matched = arithmeticOperators.contains(curr) || booleanOperators.contains(curr)
    if (matched)
      advance()
    matched
  }

  def parse(tokens: ArrayBuffer[Token]): Exp = {
    tokenStream = tokens
    parseExp
  }

  def parseExp: Exp = {
    LOG(INFO, s"parseExp: $curr")
    curr match {
        case Keyword(VAL, _, _) => parseLet
        case Keyword(LAZY, _, _) => parseLet
        case Keyword(INCLUDE, _, _) => parseInclude
        case _ => parseSimpleExp
      }
  }

  def parseLet: Exp = {
    LOG(INFO, s"parseLet: $curr")
    val token = curr
    val isLazy = matchOptional(LAZY)
    matchRequired(VAL)
    val ident = curr.tokenText
    advance()

    var letType: Type = UnknownType()
    if (matchOptional(COLON))
      letType = parseType

    matchRequired(ASSIGNMENT)
    val expValue: Exp = parseSimpleExp

    var afterExp: Exp = none
    if (matchTerminator)
      afterExp = parseExp

    Let(token, isLazy, ident, letType, expValue, afterExp)
  }

  def parseInclude: Exp = {
    LOG(INFO, s"parseInclude: $curr")
    none
  }

  def parseSimpleExp: Exp = {
    LOG(INFO, s"parseSimpleExp: $curr")
    curr match {
        case Keyword(IF, _, _) => parseBranch
        case Keyword(LIST, _, _) => parseCollectionValue
        case Keyword(ARRAY, _, _) => parseCollectionValue
        case Keyword(TUPLE, _, _) => parseCollectionValue
        case Keyword(SET, _, _) => parseCollectionValue
        case Keyword(DICT, _, _) => parseCollectionValue
        case Keyword(MATCH, _, _) => parseMatch
        case Keyword(SWITCH, _, _) => parseSwitch
        case Keyword(TYPECLASS, _, _) => parseTypeclass
        case Keyword(INSTANCE, _, _) => parseInstance
        case Keyword(TYPE, _, _) => parseAdt
        case Keyword(FN, _, _) => parseProg
        case Delimiter(Delimiters.LAMBDA, _, _) => parseLambda
        case _ => parseUtight
      }
  }

  def parseBranch: Exp = {
    LOG(INFO, s"parseBranch: $curr")
    val token = curr
    advance()
    matchRequired(LEFT_PAREN)
    val condition = parseSimpleExp
    matchRequired(RIGHT_PAREN)
    val ifBranch = parseSimpleExp

    var elseBranch: Exp = Lit(curr, NullVal())
    if (matchOptional(ELSE))
      elseBranch = parseSimpleExp

    Branch(token, condition, ifBranch, elseBranch)
  }

  def parseCollectionValue: Exp = {
    LOG(INFO, s"parseCollectionValue: $curr")
    none
  }

  def parseMatch: Exp = {
    LOG(INFO, s"parseMatch: $curr")
    none
  }

  def parseSwitch: Exp = {
    LOG(INFO, s"parseSwitch: $curr")
    none
  }

  def parseTypeclass: Exp = {
    LOG(INFO, s"parseTypeclass: $curr")
    none
  }

  def parseInstance: Exp = {
    LOG(INFO, s"parseInstance: $curr")
    none
  }

  def parseAdt: Exp = {
    LOG(INFO, s"parseAdt: $curr")
    none
  }

  def parseProg: Exp = {
    LOG(INFO, s"parseProg: $curr")
    none
  }

  def parseLambda: Exp = {
    LOG(INFO, s"parseLambda: $curr")
    none
  }

  def parseUtight: Exp = {
    LOG(INFO, s"parseUtight: $curr")
    val unaryOp = matchOperatorOptional // TODO
    parseTight
  }

  def parseTight: Exp = {
    LOG(INFO, s"parseTight: $curr")
    curr match {
      case Delimiter(LEFT_BRACE, _, _) =>
        matchRequired(LEFT_BRACE)
        val tmpExp = parseSimpleExp
        matchRequired(RIGHT_BRACE)
        tmpExp
      case _ => parseApp
    }
  }

  def parseApp: Exp = {
    LOG(INFO, s"parseApp: $curr")
    curr match {
      case Delimiter(LEFT_PAREN, _, _) =>
        matchRequired(LEFT_PAREN)
        val tmpExp = parseSimpleExp
        matchRequired(RIGHT_PAREN)
        tmpExp
      case Ident(ident, _) => none // TODO
      case _ => parsePrim
    }
  }

  def parsePrim: Exp = {
    LOG(INFO, s"parsePrim: $curr")
    curr match {
      case Keyword(TRUE, _, _) =>
        advance()
        Lit(curr, BoolVal(true)).usingType(BoolType())
      case Keyword(FALSE, _, _) =>
        advance()
        Lit(curr, BoolVal(false)).usingType(BoolType())
      case Keyword(NULL, _, _) =>
        advance()
        Lit(curr, NullVal()).usingType(NullType())
      case Value(tokenText, _) =>
        if (tokenText.head == '\"') {
          advance()
          Lit(curr, StringVal(tokenText)).usingType(StringType())
        }
        else if (tokenText.head == '\'') {
          advance()
          Lit(curr, CharVal(tokenText)).usingType(CharType())
        } else {
          advance()
          Lit(curr, IntVal(tokenText.toInt)).usingType(IntType())
        }
    }
  }

  def parseType: Type = { // TODO
    LOG(INFO, s"parseType: $curr")
    val expType = curr match {
      case Keyword(INT, _, _) =>
        advance()
        IntType()
      case Keyword(BOOL, _, _) =>
        advance()
        BoolType()
      case Keyword(CHAR, _, _) =>
        advance()
        CharType()
      case Keyword(STRING, _, _) =>
        advance()
        StringType()
      case Keyword(NULL, _, _) =>
        advance()
        NullType()
      case Keyword(LIST, _, _) =>
        advance()
        matchRequired(LEFT_BRACKET)
        val listType = parseType
        matchRequired(RIGHT_BRACKET)
        ListType(listType)
      case Keyword(ARRAY, _, _) =>
        advance()
        matchRequired(LEFT_BRACKET)
        val arrayType = parseType
        matchRequired(RIGHT_BRACKET)
        ArrayType(arrayType)
      case Keyword(SET, _, _) =>
        advance()
        matchRequired(LEFT_BRACKET)
        val setType = parseType
        matchRequired(RIGHT_BRACKET)
        SetType(setType)
      case Keyword(DICT, _, _) =>
        advance()
        matchRequired(LEFT_BRACKET)
        val keyType = parseType
        matchRequired(COMMA)
        val valueType = parseType
        matchRequired(RIGHT_BRACKET)
        DictType(keyType, valueType)
      case Keyword(TUPLE, _, _) =>
        advance()
        matchRequired(LEFT_BRACKET)
        val tupleTypes = ArrayBuffer[Type]()
        while (matchOptional(COMMA))
          tupleTypes += parseType
        matchRequired(RIGHT_BRACKET)
        TupleType(tupleTypes)
      case Delimiter(LEFT_PAREN, _, _) =>
        advance()
        matchRequired(LEFT_PAREN)
        val argTypes = ArrayBuffer[Type]()
        while (matchOptional(COMMA))
          argTypes += parseType
        matchRequired(RIGHT_PAREN)
        matchRequired(RETURN_TYPE)
        FuncType(argTypes, parseType)
      case Ident(ident, _) =>
        AdtType(ident)
      case _ =>
        reportBadType()
        UnknownType()
    }

    var returnType: Type = UnknownType()
    if (matchOptional(RETURN_TYPE)) {
      returnType = parseType
      FuncType(ArrayBuffer(expType), returnType)
    } else
      expType
  }

  def reportBadMatch(expected: String): Unit = {
    ERROR(s"Error: Expected: $expected, got ${curr.tokenText}")
    ERROR(s"Line: ${curr.fp.line + 1}, Column: ${curr.fp.column + 1}:\n")
    ERROR(s"${lineList(curr.fp.line)}")
    ERROR(s"${" " * curr.fp.column}^\n")
    errorOccurred = true
  }

  def reportBadType(): Unit = {
    ERROR(s"Error: Unexpected type: ${curr.tokenText}")
    ERROR(s"Line: ${curr.fp.line + 1}, Column: ${curr.fp.column + 1}:\n")
    ERROR(s"${lineList(curr.fp.line)}")
    ERROR(s"${" " * curr.fp.column}^\n")
    errorOccurred = true
  }
}
