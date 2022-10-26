package SemanticAnalyzer

import Lexer.SyntaxDefinitions.Delimiters
import Lexer.SyntaxDefinitions.Delimiters.{arithTypesNotPlus, logicTypes, looseComparisonTypes, plusTypes, strictComparisonTypes}
import Lexer.Token
import Logger.Level.DEBUG
import Logger.Logger.{ERROR, LOG, WARN, lineList}
import Parser.{Alias, Exp, Let, ListDef, Lit, NoOp, Prim, Ref}

import scala.collection.mutable.ArrayBuffer

object SemanticAnalyzer {
  var numErrors = 0
  var warnings = 0

  case class Environment(map: Map[String, Type])

  def addName(env: Environment, name: String, newType: Type): Environment = {
    Environment(env.map + (name -> newType))
  }

  def getName(token: Token, env: Environment, name: String): Type = {
    env.map.get(name) match {
      case s@Some(_) => s.value
      case _ =>
        reportNoSuchName(token, name)
        UnknownType()
    }
  }

  def typeConforms(token: Token, expressionType: Type, expectedType: Type, env: Environment): Type = {
    LOG(DEBUG, s"typeConforms")

    def listTypeConforms(list1: ArrayBuffer[Type], list2: ArrayBuffer[Type]): ArrayBuffer[Type] = {
      list1.zip(list2).map(t => typeConforms(token, t._1, t._2, env)).exists(t => t.isInstanceOf[UnknownType])
      list1
    }

    (expressionType, expectedType) match {
      case (_, _) if expressionType == expectedType =>
        if (expressionType == UnknownType())
          reportTypeUnknown(token)
        expressionType
      case (_, UnknownType()) => expressionType
      case (UnknownType(), _) => expectedType
      case (ListType(t1), ListType(t2)) if t1 == t2 => expressionType
      case (ArrayType(t1), ArrayType(t2)) if t1 == t2 => expressionType
      case (TupleType(lt1), TupleType(lt2)) if lt1.length == lt2.length =>
        TupleType(listTypeConforms(lt1, lt2))
      case (SetType(t1), SetType(t2)) if t1 == t2 => expressionType
      case (DictType(t1, t2), DictType(t3, t4)) if (t1 == t3) && (t2 == t4) => expressionType
      case (AdtType(i1, g1, f1), AdtType(i2, g2, f2))
        if (i1 == i2) && (g1.length == g2.length) && (f1.length == f2.length) &&
          f1.zip(f2).count(f => f._1 != f._2) == 0 =>
        AdtType(i1, listTypeConforms(g1, g2), f1)
      case (FuncType(args1, rt1), FuncType(args2, rt2)) if args1.length == args2.length =>
        FuncType(listTypeConforms(args1, args2), typeConforms(token, rt1, rt2, env))
      case _ =>
        reportTypeMismatch(token, expressionType, expectedType)
        expressionType
    }
  }

  def typeConformsOperator(token: Token, op: Delimiters.Value, left: Type, right: Type, env: Environment): Type = {
    val primType = typeConforms(token, left, right, env)
    if (primType != UnknownType()) {
      def validOperands(operatorMatch: Type => Boolean): Boolean = {
        operatorMatch(left) && operatorMatch(right) &&
          TypeUtil.isLiteralOrCollectionType(left) && TypeUtil.isLiteralOrCollectionType(right)
      }

      op match {
        case Delimiters.PLUS
          if !left.isInstanceOf[CharType] &&
            !right.isInstanceOf[CharType] &&
            validOperands(plusTypes) => left
        case Delimiters.PLUS
          if left.isInstanceOf[CharType] &&
            right.isInstanceOf[CharType] => StringType()
        case Delimiters.MINUS |
             Delimiters.MULTIPLY |
             Delimiters.DIVIDE |
             Delimiters.MODULUS
          if validOperands(arithTypesNotPlus) => left
        case Delimiters.LESS_THAN |
             Delimiters.GREATER_THAN |
             Delimiters.LESS_THAN_OR_EQUAL |
             Delimiters.GREATER_THAN_OR_EQUAL
          if validOperands(looseComparisonTypes) => BoolType()
        case Delimiters.EQUAL |
             Delimiters.NOT_EQUAL
          if validOperands(strictComparisonTypes) => BoolType()
        case Delimiters.NOT |
             Delimiters.AND |
             Delimiters.OR
          if validOperands(logicTypes) => BoolType()
        case _ =>
          reportInvalidOperands(token, op, left, right)
          left
      }
    }
    else
      primType
  }

  def typeCheck(exp: Exp, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"typeCheck")
    val evaluatedExp = eval(exp, env, expectedType)
    val expType = typeConforms(evaluatedExp.token, evaluatedExp.expType, expectedType, env)
    evaluatedExp.usingType(expType)
  }

  def eval(rootExp: Exp): Exp = {
    LOG(DEBUG, s"eval: ${rootExp.token}")
    eval(rootExp, Environment(Map[String, Type]()), UnknownType())
  }

  def eval(exp: Exp, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"eval: ${exp.token}")
    exp match {
      case Lit(_, _) => exp
      case Alias(_, _, _, _) => ???
      case let@Let(_, _, _, _, _, _) => evalLet(let, env, expectedType)
      case prim@Prim(_, _, _, _) => evalPrim(prim, env, expectedType)
      case ref@Ref(_, _) =>
        ref.usingType(typeConforms(ref.token, getName(ref.token, env, ref.ident), expectedType, env))
      case NoOp(_) => exp
      case _ =>
        reportUnexpected(exp.token)
        exp
    }
  }

  def evalAlias(alias: Alias, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"addAlias: ${alias.toString}")
    ???
  }

  def evalLet(let: Let, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalLet: ${let.ident}")
    val letValue = typeCheck(let.expValue, env, let.letType)
    val body = typeCheck(let.afterLet, addName(env, let.ident, letValue.expType), expectedType)
    Let(let.token, let.isLazy, let.ident, letValue.expType, let.expValue, body).usingType(body.expType)
  }

  def evalPrim(prim: Prim, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalPrim: ${prim.op.toString}")
    val left = typeCheck(prim.left, env, UnknownType())
    val right = typeCheck(prim.right, env, left.expType)
    val opType = typeConformsOperator(prim.token, prim.op, left.expType, right.expType, env)
    val primType = typeConforms(prim.token, opType, expectedType, env)
    Prim(prim.token, prim.op, left, right).usingType(primType)
  }

  def reportNoSuchName(token: Token, name: String): Unit = {
    ERROR(s"Error: $name does not exist in this scope")
    reportLine(token)
  }

  def reportTypeUnknown(token: Token): Unit = {
    ERROR(s"Error: Cannot deduce unknown type")
    reportLine(token)
  }

  def reportInvalidOperands(token: Token, op: Delimiters.Value, left: Type, right: Type): Unit = {
    ERROR(s"Error: Invalid operand types: ${left.printType()} and ${right.printType()} for operator \"${op.toString}\"")
    reportLine(token)
  }

  def reportTypeMismatch(token: Token, actualType: Type, expectedType: Type): Unit = {
    ERROR(s"Error: Type mismatch: ${actualType.printType()}, expected ${expectedType.printType()}")
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
    numErrors += 1
  }

  def warn(token: Token, str: String): Unit = {
    WARN(s"$str")
    WARN(s"Line: ${token.fp.line + 1}, Column: ${token.fp.column + 1}:\n")
    WARN(s"${lineList(token.fp.line)}")
    WARN(s"${" " * token.fp.column}^\n")
    warnings += 1
  }
}
