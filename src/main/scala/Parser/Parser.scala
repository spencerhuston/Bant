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
  def inBounds: Boolean = index < tokenStream.length

  def isEof: Boolean = {
    index == tokenStream.length - 1 &&
      (curr match {
        case EOF(_, _) => true
        case _ => false
      })
  }

  def isTerminator(token: Token): Boolean = {
    token match {
      case Terminator(_, _) => true
      case Delimiter(delimValue, _, _) if delimValue.toString == ";" => true
      case _ => false
    }
  }

  def matchStatementEndRequired(): Unit = {
      if (isTerminator(curr))
        advance()
      else
        reportBadMatch(curr, ";")
  }

  def matchStatementEndOptional(): Boolean = {
    if (isTerminator(curr)) {
      advance()
      true
    } else
      false
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
      reportBadMatch(curr, value.toString)

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

  def matchIdent: String = {
    if (!curr.isInstanceOf[Ident]) {
      reportBadMatch(curr, "<ident>")
      return ""
    }

    val ident = curr.tokenText

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
    if (!inBounds)
      none

    curr match {
        case Keyword(VAL, _, _) =>
          parseLet
        case Keyword(LAZY, _, _) =>
          parseLet
        case Keyword(INCLUDE, _, _) =>
          parseInclude
        case _ =>
          val smp = parseSimpleExp
          if (matchStatementEndOptional()) {
            val afterDummyLet = parseExp
            Let(smp.token, isLazy = false, dummy, smp.expType, smp, afterDummyLet)
          } else {
            smp
          }
      }
  }

  def parseLet: Let = {
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
    if (matchStatementEndOptional())
      afterExp = parseExp

    Let(token, isLazy, ident, letType, expValue, afterExp)
  }

  // TODO
  def parseInclude: Exp = {
    LOG(DEBUG, s"parseInclude: $curr")
    none
  }

  def parseSimpleExp: Exp = {
    LOG(DEBUG, s"parseSimpleExp: $curr")
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
      case Keyword(RECORD, _, _) => parseRecord
      case Keyword(ALIAS, _, _) => parseAlias
      case Keyword(FN, _, _) => parseProg
      case Delimiter(Delimiters.LAMBDA, _, _) => parseLambda
      case Delimiter(Delimiters.OR, _, _) => parseLambda
      case _ => parseUtight(0)
    }
  }

  def parseBranch: Branch = {
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
    val token = curr
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
        val mapping = ArrayBuffer[Map]()
        while (matchOptional(COMMA) || !matchOptional(RIGHT_BRACE)) {
          val key = parseSimpleExp
          matchRequired(COLON)
          val value = parseSimpleExp
          mapping += Map(key, value)
        }
        DictDef(token, mapping)
      case _ =>
        reportUnexpected(curr)
        none
    }
  }

  def parseMatch: Match = {
    LOG(DEBUG, s"parseMatch: $curr")

    val matchToken = curr
    matchRequired(MATCH)
    matchRequired(LEFT_PAREN)
    val matchVal = parseSimpleExp
    matchRequired(RIGHT_PAREN)

    matchRequired(LEFT_BRACE)
    matchRequired(CASE)
    val cases = ArrayBuffer[Case](parseCase)

    while (matchOptional(CASE)) {
      cases += parseCase
    }

    warnAnyCase(cases)

    matchRequired(RIGHT_BRACE)
    Match(matchToken, matchVal, cases)
  }

  def parseCase: Case = {
    val caseToken = curr
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
        if (matchOptional(LEFT_PAREN)) {
          while (matchOptional(COMMA) || !matchOptional(RIGHT_PAREN))
            values += parseValueCase
        }
        ConstructorCase(ident, values)
      case _ =>
        val token = curr
        parseLit match {
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
    else if (count == 0)
      warn(token, "Match is not exhaustive")

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

  def parseSuperType: Ref = {
    if (matchOptional(CASE_EXP)) Ref(curr, matchIdent)
    else Ref(curr, "$None$")
  }

  def parseTypeclass: Typeclass = {
    LOG(DEBUG, s"parseTypeclass: $curr")
    val token = curr
    matchRequired(TYPECLASS)

    var isSealed = false
    if (matchOptional(SEALED)) {
      isSealed = true
    }

    val typeclassIdent = matchIdent
    val genericTypes = parseGenerics
    val superclass = parseSuperType

    matchRequired(LEFT_BRACE)
    val signatures = ArrayBuffer[Signature]()
    while (matchOptional(COMMA) || !matchOptional(RIGHT_BRACE)) {
      val name = Ref(curr, matchIdent)
      matchRequired(ASSIGNMENT)
      val signature = parseType
      signatures += Signature(name, signature)
    }
    matchStatementEndRequired()

    Typeclass(token, isSealed, typeclassIdent, genericTypes, superclass, signatures, parseExp)
  }

  def parseInstance: Instance = {
    LOG(DEBUG, s"parseInstance: $curr")
    val token = curr
    matchRequired(INSTANCE)
    val adtIdent = Ref(curr, matchIdent)
    matchRequired(COLON)

    val typeclassIdents = ArrayBuffer[Ref]()
    typeclassIdents += Ref(curr, matchIdent)
    while (matchOptional(COMMA)) {
      typeclassIdents += Ref(curr, matchIdent)
    }

    matchRequired(LEFT_BRACE)
    val funcs = ArrayBuffer[FunDef]()
    while (matchOptional(FN)) {
      funcs += parseFunDef
    }
    matchRequired(RIGHT_BRACE)
    matchStatementEndRequired()
    Instance(token, adtIdent, typeclassIdents, funcs, parseExp)
  }

  def parseAdt: Adt = {
    LOG(DEBUG, s"parseAdt: $curr")
    val token = curr
    matchRequired(TYPE)
    val ident = matchIdent
    val generics = parseGenerics

    var derivedFrom = Ref(curr, "$None$")
    if (matchOptional(DERIVES)) {
      derivedFrom = Ref(curr, matchIdent)
    }

    matchRequired(LEFT_BRACE)

    val constructors = ArrayBuffer[Constructor]()
    while (matchOptional(LAMBDA) || !matchOptional(RIGHT_BRACE)) {
      val typeList = ArrayBuffer[Type](parseType)
      while (matchOptional(COMMA) && !matchOptional(LAMBDA) && !matchOptional(RIGHT_BRACE)) {
        typeList += parseType
      }
      constructors += Constructor(typeList)
    }

    matchStatementEndRequired()
    Adt(token, ident, generics, derivedFrom, constructors, parseExp)
  }

  def parseRecord: Exp = {
    LOG(DEBUG, s"parseRecord: $curr")
    val token = curr
    matchRequired(RECORD)

    var isSealed = false
    if (matchOptional(SEALED)) {
      isSealed = true
    }

    val ident = matchIdent
    val generics = parseGenerics
    val superType = parseSuperType

    var derivedFrom = Ref(curr, "$None$")
    if (matchOptional(DERIVES)) {
      derivedFrom = Ref(curr, matchIdent)
    }

    matchRequired(LEFT_BRACE)

    val members = ArrayBuffer[Member]()
    while (matchOptional(COMMA) || !matchOptional(RIGHT_BRACE)) {
      val name = matchIdent
      matchRequired(COLON)
      val memberType = parseType
      members += Member(name, memberType)
    }

    matchStatementEndRequired()
    Record(token, isSealed, ident, generics, superType, derivedFrom, members, parseExp)
  }

  def parseAlias: Alias = {
    LOG(DEBUG, s"parseAlias: $curr")
    val token = curr
    matchRequired(ALIAS)
    val alias = matchIdent
    matchRequired(ASSIGNMENT)
    val actualType = parseType
    matchStatementEndRequired()
    Alias(token, alias, actualType, parseExp)
  }

  def parseProg: Prog = {
    LOG(DEBUG, s"parseProg: $curr")
    val token = curr
    val funcs = ArrayBuffer[FunDef]()
    while (matchOptional(FN)) {
      funcs += parseFunDef
    }
    Prog(token, funcs, parseExp)
  }

  def parseFunDef: FunDef = {
    LOG(DEBUG, s"parseFunDef: $curr")

    val token = curr
    val ident = matchIdent
    val genericTypes = parseGenerics

    matchRequired(LEFT_PAREN)
    val params = ArrayBuffer[Parameter]()
    while (matchOptional(COMMA) || !matchOptional(RIGHT_PAREN)) {
      params += parseParameter
    }

    matchRequired(RETURN_TYPE)
    val returnType = parseType

    matchRequired(ASSIGNMENT)
    val body = parseSimpleExp
    matchStatementEndRequired()

    FunDef(token, ident, genericTypes, params, returnType, body)
  }

  def parseGenerics: ArrayBuffer[Generic] = {
    val genericTypes = ArrayBuffer[Generic]()
    if (matchOptional(LEFT_BRACKET)) {
      while (matchOptional(COMMA) || !matchOptional(RIGHT_BRACKET)) {
        val genericType = matchIdent
        var lowerBoundType = ""
        var upperBoundType = ""

        if (matchOptional(LOWER_BOUND)) {
          lowerBoundType = matchIdent
        }
        if (matchOptional(UPPER_BOUND)) {
          upperBoundType = matchIdent
        }
        genericTypes += Generic(genericType, lowerBoundType, upperBoundType)
      }
    }
    genericTypes
  }

  def parseParameter: Parameter = {
    LOG(DEBUG, s"parseArg: $curr")
    val token = curr
    val ident = matchIdent
    matchRequired(COLON)
    val paramType = parseType

    var defaultVal: Exp = none
    if (matchOptional(ASSIGNMENT))
      defaultVal = parseDefaultValue

    Parameter(token, ident, paramType, defaultVal)
  }

  def parseDefaultValue: Exp = {
    curr match {
      case Keyword(LIST, _, _) => parseCollectionValue
      case Keyword(ARRAY, _, _) => parseCollectionValue
      case Keyword(SET, _, _) => parseCollectionValue
      case Keyword(TUPLE, _, _) => parseCollectionValue
      case Keyword(DICT, _, _) => parseCollectionValue
      case _ => parseAtom
    }
  }

  def parseLambda: Let = {
    LOG(DEBUG, s"parseLambda: $curr")
    val token = curr
    var emptyParams = false

    if (matchOptional(OR)) {
      emptyParams = true
    } else
      matchOptional(LAMBDA)

    val ident = anon
    val params = ArrayBuffer[Parameter]()
    while (!emptyParams && (matchOptional(COMMA) || !matchOptional(LAMBDA))) {
      params += parseParameter
    }

    matchRequired(RETURN_TYPE)
    val returnType: Type = parseType
    matchRequired(ASSIGNMENT)
    val body = parseSimpleExp
    matchStatementEndRequired()

    Let(token,
        isLazy = false,
        ident,
        UnknownType(),
        FunDef(token, ident + "_def", ArrayBuffer[Generic](), params, returnType, body),
        Ref(token, ident + "_def"))
  }

  def parseUtight(min: Int): Exp = {
    LOG(DEBUG, s"parseUtight(Int): $curr")
    var leftSide = parseUtight

    while (isBinaryOperator(min)) {
      val op = curr.asInstanceOf[Delimiter].delim
      val tempMin = getPrecedence(op) + 1
      advance()
      val rightSide = parseUtight(tempMin)
      leftSide = Prim(leftSide.token, op, leftSide, rightSide)
    }
    leftSide
  }

  def parseUtight: Exp = {
    LOG(DEBUG, s"parseUtight: $curr")
    val token = curr
    var op = STATEMENT_END
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
      case _ => parseApplication
    }
  }

  def parseArguments: ArrayBuffer[Exp] = {
    LOG(DEBUG, s"parseArguments: $curr")
    val arguments = ArrayBuffer[Exp]()
    while (matchOptional(COMMA) || !matchOptional(RIGHT_PAREN)) {
      arguments += parseSimpleExp
    }
    arguments
  }

  def parseApplication: Exp = {
    LOG(DEBUG, s"parseApplication: $curr")
    val token = curr
    val ident = parseAtom

    if (ident.isInstanceOf[Lit] || ident.isInstanceOf[NoOp])
      return ident

    val genericParameters = ArrayBuffer[Type]()
    if (matchOptional(LEFT_BRACKET)) {
      while (matchOptional(COMMA) || !matchOptional(RIGHT_BRACKET)) {
        genericParameters += parseType
      }
    }

    var arguments = ArrayBuffer[Exp]()
    if (matchOptional(LEFT_PAREN)) {
      arguments = parseArguments
      var app = App(token, ident, genericParameters, arguments)

      while (matchOptional(LEFT_PAREN)) {
        val outerArguments = parseArguments
        app = App(token, app, genericParameters, outerArguments)
      }
      app
    }
    else
      ident
  }

  def parseTupleAccessIndex: IntVal = {
    LOG(DEBUG, s"parseTupleAccessIndex: $curr")
    val prim = parseLit
    if (!prim.isInstanceOf[Lit] && !prim.asInstanceOf[Lit].value.isInstanceOf[IntVal]) {
      reportBadMatch(curr, "<IntVal>", "Tuple access requires integer literal")
      return IntVal(-1)
    }
    prim.asInstanceOf[Lit].value.asInstanceOf[IntVal]
  }

  def parseAtom: Exp = {
    LOG(DEBUG, s"parseAtom: $curr")
    curr match {
      case Delimiter(LEFT_PAREN, _, _) =>
        matchRequired(LEFT_PAREN)
        val tmpExp = parseSimpleExp
        matchRequired(RIGHT_PAREN)
        tmpExp
      case Ident(_, _) =>
        val ref = Ref(curr, matchIdent)
        if (matchOptional(ACCESS)) {
          val accessIndex = parseTupleAccessIndex
          var tupleAccess = TupleAccess(curr, ref, accessIndex)

          while (matchOptional(ACCESS)) {
            tupleAccess = TupleAccess(curr, tupleAccess, parseTupleAccessIndex)
          }
          return tupleAccess
        }
        ref
      case _ => parseLit
    }
  }

  def parseLit: Exp = {
    LOG(DEBUG, s"parseLit: $curr")
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
      case EOF(_, _) => none
      case _ =>
        reportUnexpected(curr)
        advance()
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
        advance()
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
    val anonIdent = s"anon$$$anonCount"
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
    ERROR(s"Unexpected: ${token.tokenText}")
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
