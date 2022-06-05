package Parser

import Lexer.SyntaxDefinitions.Delimiters._
import Lexer.SyntaxDefinitions.Delimiters
import Lexer.SyntaxDefinitions.Keywords._
import Lexer.{Delimiter, EOF, Ident, Keyword, Terminator, Token, Value}
import Logger.Level.DEBUG
import Logger.Logger.{ERROR, LOG, WARN, lineList}
import TypeChecker._

import scala.collection.mutable.ArrayBuffer

object Parser {
  var index = 0
  var tokenStream: ArrayBuffer[Token] = ArrayBuffer[Token]()
  var errorOccurred = false

  var dummyCount = 0
  var anonCount = 0

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

  def getToken: Token = {
    skipTerminator()
    curr
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
      reportBadMatch(curr, value.toString)

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
      case Delimiter(delim, _, _) if delim.toString == ";" => true
      case _ => false
    }

    if (matched)
      advance()

    matched
  }

  def matchIdent: String = {
    skipTerminator()
    if (!curr.isInstanceOf[Ident]) {
      reportBadMatch(curr, "<ident>")
      return ""
    }

    val ident = curr.tokenText

    if (ident == "_") {
      ERROR(s"Error: _ reserved for matches and lambdas")
      reportLine(curr)
    }

    advance()
    ident
  }

  def isBinaryOperator(min: Int): Boolean = {
    curr match {
      case Delimiter(delim, _, _) =>
        (arithmeticOperators.contains(delim) || booleanOperators.contains(delim)) &&
        getPrecedence(delim) >= min
      case _ => false
    }
  }

  def getPrecedence(op: Delimiters.Value): Int = {
    if (op == AND || op == OR) 0
    else if (op == PLUS || op == MINUS) 2
    else if (op == MULTIPLY || op == DIVIDE || op == MODULUS) 3
    else 1
  }

  def parse(tokens: ArrayBuffer[Token]): Exp = {
    tokenStream = tokens
    skipTerminator()

    try {
      parseExp
    } catch {
      case _: IndexOutOfBoundsException =>
        ERROR(s"Error: EOF encountered during parsing")
        index = 0
        none
    }
  }

  def parseExp: Exp = {
    LOG(DEBUG, s"parseExp: $curr")
    curr match {
        case Keyword(VAL, _, _) => parseLet
        case Keyword(LAZY, _, _) => parseLet
        case Keyword(INCLUDE, _, _) => parseInclude
        case _ => {
          val smp = parseSimpleExp
          if (matchTerminator) {
            skipTerminator()
            Let(smp.token, isLazy = false, dummy, smp.expType, smp, parseExp)
          } else {
            smp
          }
        }
      }
  }

  def parseLet: Exp = {
    LOG(DEBUG, s"parseLet: $curr")
    val token = getToken
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

  // TODO
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
        case Keyword(SET, _, _) => parseCollectionValue
        case Keyword(TUPLE, _, _) => parseCollectionValue
        case Keyword(DICT, _, _) => parseCollectionValue
        case Keyword(MATCH, _, _) => parseMatch
        case Keyword(TYPECLASS, _, _) => parseTypeclass
        case Keyword(INSTANCE, _, _) => parseInstance
        case Keyword(TYPE, _, _) => parseAdt
        case Keyword(FN, _, _) => parseProg
        case Delimiter(Delimiters.LAMBDA, _, _) => parseLambda
        case _ => parseUtight(0)
      }
  }

  def parseBranch: Exp = {
    LOG(DEBUG, s"parseBranch: $curr")
    val token = getToken
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
    val token = getToken
    curr match {
      case Keyword(LIST, _, _) =>
        advance()
        matchRequired(LEFT_BRACE)
        val values = ArrayBuffer[Exp]()
        while (matchOptional(COMMA) || !matchOptional(RIGHT_BRACE))
          values += parseSimpleExp
        ListDef(token, values)
      case Keyword(ARRAY, _, _) =>
        advance()
        matchRequired(LEFT_BRACE)
        val values = ArrayBuffer[Exp]()
        while (matchOptional(COMMA) || !matchOptional(RIGHT_BRACE))
          values += parseSimpleExp
        ArrayDef(token, values)
      case Keyword(SET, _, _) =>
        advance()
        matchRequired(LEFT_BRACE)
        val values = ArrayBuffer[Exp]()
        while (matchOptional(COMMA) || !matchOptional(RIGHT_BRACE))
          values += parseSimpleExp
        SetDef(token, values)
      case Keyword(TUPLE, _, _) =>
        advance()
        matchRequired(LEFT_BRACE)
        val values = ArrayBuffer[Exp]()
        while (matchOptional(COMMA) || !matchOptional(RIGHT_BRACE))
          values += parseSimpleExp
        TupleDef(token, values)
      case Keyword(DICT, _, _) =>
        advance()
        matchRequired(LEFT_BRACE)
        val keys = ArrayBuffer[Exp]()
        val values = ArrayBuffer[Exp]()
        while (matchOptional(COMMA) || !matchOptional(RIGHT_BRACE)) {
          keys += parseSimpleExp
          matchRequired(COLON)
          values += parseSimpleExp
        }
        DictDef(token, keys, values)
    }
  }

  def parseMatch: Exp = {
    LOG(DEBUG, s"parseMatch: $curr")

    val matchToken = getToken
    matchRequired(MATCH)
    matchRequired(LEFT_PAREN)
    val matchVal = parseSimpleExp
    matchRequired(RIGHT_PAREN)

    matchRequired(LEFT_BRACE)
    val cases = ArrayBuffer[Case](parseCase)

    while (!peek(RIGHT_BRACE))
      cases += parseCase

    warnAnyCase(cases)

    matchRequired(RIGHT_BRACE)
    Match(matchToken, matchVal, cases)
  }

  def parseCase: Case = {
    matchRequired(CASE)
    val caseToken = getToken
    val casePattern = parseCasePattern
    matchRequired(CASE_EXP)
    val caseExp = parseSimpleExp
    Case(caseToken, casePattern, caseExp)
  }

  def parseCasePattern: CasePattern = {
    LOG(DEBUG, s"parseCaseValue: $curr")
    curr match {
      case Ident(tokenText, _) if peek(COLON) =>
        val ident = tokenText
        advance()
        matchRequired(COLON)
        val caseValType = parseType
        TypeCase(ident, caseValType)
      case _ =>
        ValueCase(parseValueCase)
    }
  }

  def parseValueCase: ValueCasePattern = {
    curr match {
      case Ident(tokenText, _) if tokenText == "_" =>
        advance()
        AnyCase()
      case Ident(tokenText, _) =>
        val ident = tokenText
        advance()
        val values = ArrayBuffer[ValueCasePattern]()
        matchRequired(LEFT_PAREN)

        while (matchOptional(COMMA) || !matchOptional(RIGHT_PAREN))
          values += parseValueCase

        ConstructorCase(ident, values)
      case _ =>
        val token = getToken
        parsePrim match {
          case NoOp(_) =>
            reportBadMatch(token, "<primitive>")
            advance()
            AnyCase()
          case l: Lit =>
            LitCase(l.value)
        }
    }
  }

  def warnAnyCase(cases: ArrayBuffer[Case]): Unit = {
    var token = cases(0).token
    var count = 0
    cases.foreach((c: Case) => {
      c.casePattern match {
        case tc: TypeCase if tc.ident == "_" => count += 1; token = c.token
        case vc: ValueCase if vc.value.isInstanceOf[AnyCase] => count += 1; token = c.token
        case _ => 0
      }
    })

    if (count > 1)
      warn(token, "Wildcard occurs more than once")

    token = cases(0).token
    if (cases.indexWhere((c: Case) => {
      c.casePattern match {
        case vc: ValueCase if vc.value.isInstanceOf[AnyCase] => token = c.token; true
        case _ => false
      }
    }) != cases.length - 1) {
      warn(token, "Wildcard is not last case pattern")
    }
  }

  // TODO
  def parseTypeclass: Exp = {
    LOG(DEBUG, s"parseTypeclass: $curr")
    none
  }

  // TODO
  def parseInstance: Exp = {
    LOG(DEBUG, s"parseInstance: $curr")
    matchRequired(INSTANCE)
    val adtIdent = matchIdent // TODO
    matchRequired(COLON)
    val typeclassIdent = matchIdent // TODO
    matchRequired(LEFT_BRACE)
    val prog = parseProg
    matchRequired(RIGHT_BRACE)
    ???
  }

  // TODO
  def parseAdt: Exp = {
    LOG(DEBUG, s"parseAdt: $curr")
    none
  }

  // TODO
  def parseProg: Exp = {
    LOG(DEBUG, s"parseProg: $curr")
    none
  }

  // TODO
  def parseFunDef: FunDef = {
    LOG(DEBUG, s"parseFunDef: $curr")
    ???
  }

  // TODO
  def parseArg: Arg = {
    LOG(DEBUG, s"parseArg: $curr")
    ???
  }

  // TODO
  def parseLambda: Exp = {
    LOG(DEBUG, s"parseLambda: $curr")
    none
  }

  def parseUtight(min: Int): Exp = {
    LOG(DEBUG, s"parseUtight(Int): $curr")
    var leftSide = parseUtight

    if (isBinaryOperator(min)) {
      val op = curr.asInstanceOf[Delimiter].delim
      val tempMin = getPrecedence(op) + 1
      advance()
      leftSide = Prim(leftSide.token, op, leftSide, parseUtight(tempMin))
    }
    leftSide
  }

  def parseUtight: Exp = {
    LOG(DEBUG, s"parseUtight: $curr")
    val token = getToken
    var op = SEMI_COLON
    if (matchOptional(NOT))
      op = NOT
    else if (matchOptional(MINUS))
      op = MINUS

    val rightSide = parseTight
    if (op == NOT) {
      Prim(token, AND, Lit(token, BoolVal(false)), rightSide)
    } else if (op == MINUS) {
      Prim(token, MINUS, Lit(token, IntVal(0)), rightSide)
    } else
      rightSide
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
      case _ =>
        reportUnexpected(curr)
        none
    }
  }

  def parseType: Type = {
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
        matchRequired(LEFT_PAREN)
        val argTypes = ArrayBuffer[Type]()
        while (matchOptional(COMMA) || !matchOptional(RIGHT_PAREN))
          argTypes += parseType
        matchRequired(RIGHT_PAREN)
        matchRequired(RETURN_TYPE)
        FuncType(argTypes, parseType)
      case Ident(ident, _) =>
        advance()
        val generics = ArrayBuffer[Type]()
        if (matchOptional(LEFT_BRACKET)) {
          generics += parseType
          while (matchOptional(COMMA) || !matchOptional(RIGHT_BRACKET))
            generics += parseType
        }
        AdtType(ident, generics)
      case _ =>
        reportBadType(curr)
        UnknownType()
    }

    var returnType: Type = UnknownType()
    if (matchOptional(RETURN_TYPE)) {
      returnType = parseType
      FuncType(ArrayBuffer(expType), returnType)
    } else
      expType
  }

  def dummy: String = {
    val dummyIdent = s"dummy$$$dummyCount"
    dummyCount += 1
    dummyIdent
  }

  def anon: String = {
    val anonIdent = s"dummy$$$anonCount"
    anonCount += 1
    anonIdent
  }

  def reportBadMatch(token: Token, expected: String, note: String = ""): Unit = {
    ERROR(s"Error: Expected: $expected, got ${token.tokenText}")
    if (note.nonEmpty)
      ERROR(s"($note)")
    reportLine(token)
  }

  def reportBadType(token: Token): Unit = {
    ERROR(s"Error: Unexpected type: ${token.tokenText}")
    reportLine(token)
  }

  def reportUnexpected(token: Token): Unit = {
    ERROR(s"Unexpected : ${token.tokenText}")
    reportLine(token)
  }

  def reportLine(token: Token): Unit = {
    ERROR(s"Line: ${token.fp.line + 1}, Column: ${token.fp.column + 1}:\n")
    ERROR(s"${lineList(token.fp.line)}")
    ERROR(s"${" " * token.fp.column}^\n")
    errorOccurred = true
  }

  def warn(token: Token, str: String): Unit = {
    WARN(s"$str")
    WARN(s"Line: ${token.fp.line + 1}, Column: ${token.fp.column + 1}:\n")
    WARN(s"${lineList(token.fp.line)}")
    WARN(s"${" " * token.fp.column}^\n")
  }
}
