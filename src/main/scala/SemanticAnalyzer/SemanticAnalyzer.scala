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

  def analyze(rootExp: Exp): Exp = {
    LOG(DEBUG, s"parseType: ${rootExp.token}")
    eval(rootExp, Environment(Map[String, Type]()), UnknownType())
  }

  def eval(exp: Exp, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"eval: ${exp.token}")
    print(TupleType(ArrayBuffer[Type](IntType(), BoolType(), CharType())).printType())
    exp match {
      case l@Lit(_, _) => checkType(l.token, l.expType, expectedType); l
      case Alias(_, _, _, _) => addAlias(exp.asInstanceOf[Alias], env, expectedType)
      case Let(_, _, _, _, _, _) => evalLet(exp.asInstanceOf[Let], env, expectedType)
      case r@Ref(_, _) => checkType(r.token, getName(r.token, env, r.ident), expectedType); r
      case n@NoOp(_) => n
      case _ =>
        reportUnexpected(exp.token)
        exp
    }
  }

  def addAlias(alias: Alias, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"addAlias: ${alias.toString}")
    eval(alias.afterAlias, addName(env, alias.alias, alias.actualType), expectedType)
  }

  def evalLet(let: Let, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalLet: ${let.ident}")
    eval(let.expValue, env, let.letType)
    eval(let.afterLet, addName(env, let.ident, let.letType), expectedType)
  }

  def checkType(token: Token, actualType: Type, expectedType: Type): Unit = {
    LOG(DEBUG, s"checkType: ${actualType.toString}, ${expectedType.toString}")
    if (actualType != expectedType)
      reportTypeMismatch(token, actualType, expectedType)
  }

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

  def reportNoSuchName(token: Token, name: String): Unit = {
    ERROR(s"Error: $name does not exist in this scope")
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
