package SemanticAnalyzer

import Lexer.Token
import Logger.Level.DEBUG
import Logger.Logger.{ERROR, LOG, WARN, lineList}
import Parser.{Alias, BoolVal, CharVal, Exp, IntVal, Let, Lit, NoOp, NullVal, Ref, StringVal}

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
    (expressionType, expectedType) match {
      case (_, _) if expressionType == expectedType => expressionType
      case (t1, UnknownType()) => t1
      case (UnknownType(), t2) => t2
      case (ListType(t1), ListType(t2)) => ???
      case (ArrayType(t1), ArrayType(t2)) => ???
      case (TupleType(lt1), TupleType(lt2)) => ???
      case (SetType(t1), SetType(t2)) => ???
      case (DictType(t1, t2), DictType(t3, t4)) => ???
      case (AdtType(i1, g1, f1), AdtType(i2, g2, f2)) => ???
      case (FuncType(args1, rt1), FuncType(args2, rt2)) if args1.length == args2.length => ???
      case _ =>
        reportTypeMismatch(token, expressionType, expectedType)
        UnknownType()
    }
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

  def reportNoSuchName(token: Token, name: String): Unit = {
    ERROR(s"Error: $name does not exist in this scope")
    reportLine(token)
  }

  def reportTypeMismatch(token: Token, actualType: Type, expectedType: Type): Unit = {
    ERROR(s"Error: Type mismatch: ${actualType.printType()}, expected ${expectedType.printType()}")
    reportLine(token)
  }

  def reportTypeUnknown(token: Token): Unit = {
    ERROR(s"Error: Cannot deduce unknown type")
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
