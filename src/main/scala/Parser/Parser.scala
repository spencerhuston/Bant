package Parser

import Lexer.SyntaxDefinitions.Delimiters._
import Lexer.SyntaxDefinitions.Delimiters
import Lexer.SyntaxDefinitions.Keywords._
import Lexer.{Delimiter, EOF, Ident, Keyword, Terminator, Token, Value}
import Logger.Level.DEBUG
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

  def skipTerminator(): Unit = {
    while (matchTerminator)
      ()
  }

  def matchRequired[T <: Enumeration#Value](value: T): Boolean = {
    skipTerminator()
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
    skipTerminator()
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

  def peek[T <: Enumeration#Value](value: T): Boolean = {
    if (index < tokenStream.length - 1) {
      tokenStream(index + 1) match
      {
        case Keyword(keywordValue, _, _) =>
          keywordValue == value
        case Delimiter(delimValue, _, _) =>
          delimValue == value
        case _ => false
      }
    }
    else
      false
  }

  def matchTerminator: Boolean = {
    val matched = curr match {
      case Terminator(_, _) => true
      case _ => false
    }

    if (matched)
      advance()

    matched
  }

  def matchIdent: String = {
    skipTerminator()
    if (!curr.isInstanceOf[Ident]) {
      reportBadMatch("<ident>")
      return ""
    }

    val ident = curr.tokenText
    advance()
    ident
  }

  def matchOperatorOptional: Boolean = {
    skipTerminator()
    val matched = arithmeticOperators.contains(curr) || booleanOperators.contains(curr)
    if (matched)
      advance()
    matched
  }

  def parse(tokens: ArrayBuffer[Token]): Exp = {
    tokenStream = tokens
    skipTerminator()
    parseExp
  }

  def parseExp: Exp = {
    LOG(DEBUG, s"parseExp: $curr")
    curr match {
        case Keyword(VAL, _, _) => parseLet
        case Keyword(LAZY, _, _) => parseLet
        case Keyword(INCLUDE, _, _) => parseInclude
        case _ => parseSimpleExp
      }
  }

  def parseLet: Exp = {
    LOG(DEBUG, s"parseLet: $curr")
    val token = curr
    val isLazy = matchOptional(LAZY)
    matchRequired(VAL)
    val ident = matchIdent

    var letType: Type = UnknownType()
    if (matchOptional(COLON))
      letType = parseType

    matchRequired(ASSIGNMENT)
    val expValue: Exp = parseSimpleExp

    var afterExp: Exp = none
    if (matchTerminator) {
      skipTerminator()
      afterExp = parseExp
    }

    Let(token, isLazy, ident, letType, expValue, afterExp)
  }

  def parseInclude: Exp = {
    LOG(DEBUG, s"parseInclude: $curr")
    none
  }

  def parseSimpleExp: Exp = {
    LOG(DEBUG, s"parseSimpleExp: $curr")
    skipTerminator()
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
    LOG(DEBUG, s"parseBranch: $curr")
    val token = curr
    matchRequired(IF)
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
    LOG(DEBUG, s"parseCollectionValue: $curr")
    none
  }

  def parseMatch: Exp = {
    LOG(DEBUG, s"parseMatch: $curr")

    val matchToken = curr
    matchRequired(MATCH)
    matchRequired(LEFT_PAREN)

    val ident = Ref(curr, matchIdent)
    matchRequired(RIGHT_PAREN)
    matchRequired(LEFT_BRACE)

    val cases = ArrayBuffer[MatchCase]()

    var caseToken = curr
    matchRequired(CASE)
    var caseType = parseType
    matchRequired(CASE_EXP)
    var caseExp = parseSimpleExp
    cases += MatchCase(caseToken, caseType, caseExp)

    while (!peek(RIGHT_BRACE)) {
      caseToken = curr
      matchRequired(CASE)
      caseType = parseType
      matchRequired(CASE_EXP)
      caseExp = parseSimpleExp
      cases += MatchCase(caseToken, caseType, caseExp)
    }

    matchRequired(RIGHT_BRACE)
    Match(matchToken, ident, cases)
  }

  def parseSwitch: Exp = {
    LOG(DEBUG, s"parseSwitch: $curr")

    val switchToken = curr
    matchRequired(SWITCH)
    matchRequired(LEFT_PAREN)
    val switchAtom = parseAtom
    matchRequired(RIGHT_PAREN)
    matchRequired(LEFT_BRACE)

    val cases = ArrayBuffer[SwitchCase]()

    var caseToken = curr
    matchRequired(CASE)
    var caseAtom = parseAtom
    matchRequired(CASE_EXP)
    var caseExp = parseSimpleExp
    cases += SwitchCase(caseToken, caseAtom, caseExp)

    while (!peek(RIGHT_BRACE)) {
      caseToken = curr
      matchRequired(CASE)
      caseAtom = parseAtom
      matchRequired(CASE_EXP)
      caseExp = parseSimpleExp
      cases += SwitchCase(caseToken, caseAtom, caseExp)
    }

    matchRequired(RIGHT_BRACE)
    Switch(switchToken, switchAtom, cases)
  }

  def parseTypeclass: Exp = {
    LOG(DEBUG, s"parseTypeclass: $curr")
    none
  }

  def parseInstance: Exp = {
    LOG(DEBUG, s"parseInstance: $curr")
    none
  }

  def parseAdt: Exp = {
    LOG(DEBUG, s"parseAdt: $curr")
    none
  }

  def parseProg: Exp = {
    LOG(DEBUG, s"parseProg: $curr")
    none
  }

  def parseLambda: Exp = {
    LOG(DEBUG, s"parseLambda: $curr")
    none
  }

  def parseUtight: Exp = {
    LOG(DEBUG, s"parseUtight: $curr")
    val unaryOp = matchOperatorOptional // TODO
    parseTight
  }

  def parseTight: Exp = {
    LOG(DEBUG, s"parseTight: $curr")
    curr match {
      case Delimiter(LEFT_BRACE, _, _) =>
        matchRequired(LEFT_BRACE)
        val tmpExp = parseSimpleExp
        matchRequired(RIGHT_BRACE)
        tmpExp
      case _ => parseAtom
    }
  }

  def parseAtom: Exp = {
    LOG(DEBUG, s"parseAtom: $curr")
    curr match {
      case Delimiter(LEFT_PAREN, _, _) =>
        matchRequired(LEFT_PAREN)
        val tmpExp = parseSimpleExp
        matchRequired(RIGHT_PAREN)
        tmpExp
      case Ident(ident, _) =>
        val token = curr
        advance()
        Ref(token, ident)
      case _ => parsePrim
    }
  }

  def parsePrim: Exp = {
    LOG(DEBUG, s"parsePrim: $curr")
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
      case Terminator(_, _) => none
    }
  }

  def parseType: Type = { // TODO
    LOG(DEBUG, s"parseType: $curr")
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
        val tupleTypes = ArrayBuffer[Type](parseType)
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

  def reportBadMatch(expected: String, note: String = ""): Unit = {
    ERROR(s"Error: Expected: $expected, got ${curr.tokenText}")
    if (note.nonEmpty)
      ERROR(s"($note)")
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
