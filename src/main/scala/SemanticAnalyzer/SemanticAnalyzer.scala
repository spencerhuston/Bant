package SemanticAnalyzer

import Lexer.SyntaxDefinitions.Delimiters
import Lexer.SyntaxDefinitions.Delimiters.{arithTypesNotPlus, logicTypes, looseComparisonTypes, plusTypes, strictComparisonTypes}
import Lexer.Token
import Logger.Level.DEBUG
import Logger.Logger.{ERROR, LOG, WARN, lineList}
import Parser.{Adt, Alias, ArrayDef, Branch, DictDef, Exp, Let, ListDef, Lit, Match, NoOp, Prim, Ref, SetDef, TupleDef}

import scala.collection.mutable.ArrayBuffer

object SemanticAnalyzer {
  var numErrors = 0
  var warnings = 0

  case class Environment(map: Map[String, Type], aliases: Map[String, Type])

  def addName(env: Environment, name: String, newType: Type): Environment = {
    Environment(env.map + (name -> newType), env.aliases)
  }

  def getName(token: Token, env: Environment, name: String): Type = {
    env.map.get(name) match {
      case s@Some(_) => s.value
      case _ =>
        reportNoSuchName(token, name)
        UnknownType()
    }
  }

  def addAlias(env: Environment, name: String, newType: Type): Environment = {
    Environment(env.map, env.aliases + (name -> newType))
  }

  def getAlias(token: Token, env: Environment, name: String): Type = {
    env.aliases.get(name) match {
      case s@Some(_) => s.value
      case _ =>
        reportNoSuchName(token, name)
        UnknownType()
    }
  }

  def typeConforms(token: Token, expressionType: Type, expectedType: Type, env: Environment): Type = {
    LOG(DEBUG, s"typeConforms")

    def typeWellFormed(t: Type): Type = t match {
      case ListType(lt) => ListType(typeWellFormed(lt))
      case ArrayType(at) => ArrayType(typeWellFormed(at))
      case SetType(st) => SetType(typeWellFormed(st))
      case TupleType(tts) => TupleType(tts.map(typeWellFormed))
      case DictType(kt, vt) => DictType(typeWellFormed(kt), typeWellFormed(vt))
      case AdtUseType(_, _, _) => ??? // TODO
      case FuncType(_, _) => ??? // TODO
      case UnknownType() =>
        reportTypeUnknown(token)
        t
      case _ => t
    }

    def listTypeConforms(list1: ArrayBuffer[Type], list2: ArrayBuffer[Type]): ArrayBuffer[Type] = {
      list1.zip(list2).map(t => typeConforms(token, t._1, t._2, env)).exists(t => t.isInstanceOf[UnknownType])
      list1
    }

    (expressionType, expectedType) match {
      case (_, _) if expressionType == expectedType =>
        if (expressionType == UnknownType())
          reportTypeUnknown(token)
        typeWellFormed(expressionType)
      case (ListType(t1), ListType(t2)) => ListType(typeConforms(token, t1, t2, env))
      case (ArrayType(t1), ArrayType(t2)) => ArrayType(typeConforms(token, t1, t2, env))
      case (SetType(t1), SetType(t2)) => SetType(typeConforms(token, t1, t2, env))
      case (TupleType(lt1), TupleType(lt2)) if lt1.isEmpty => TupleType(lt2)
      case (TupleType(lt1), TupleType(lt2)) if lt2.isEmpty => TupleType(lt1)
      case (TupleType(lt1), TupleType(lt2)) if lt2.isEmpty => TupleType(listTypeConforms(lt1, lt2))
      case (DictType(t1, t2), DictType(t3, t4)) =>
        DictType(typeConforms(token, t1, t3, env), typeConforms(token, t2, t4, env))
      case (AdtUseType(i1, g1, f1), AdtUseType(i2, g2, f2))
        if (i1 == i2) && (g1.length == g2.length) && (f1.length == f2.length) &&
          f1.zip(f2).count(f => f._1 != f._2) == 0 =>
        AdtUseType(i1, g1, f1) // TODO: FOR ADT APP
      case (AdtUseType(i, _, _), t2) => typeConforms(token, getAlias(token, env, i), t2, env)
      case (t1, AdtUseType(i, _, _)) => typeConforms(token, t1, getAlias(token, env, i), env)
      case (FuncType(args1, rt1), FuncType(args2, rt2)) if args1.length == args2.length =>
        FuncType(listTypeConforms(args1, args2), typeConforms(token, rt1, rt2, env))
      case (_, UnknownType()) => typeWellFormed(expressionType)
      case (UnknownType(), _) => typeWellFormed(expectedType)
      case _ =>
        reportTypeMismatch(token, expressionType, expectedType)
        expressionType
    }
  }

  def typesConformToOperator(token: Token, op: Delimiters.Value, left: Type, right: Type, env: Environment): Type = {
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
    eval(rootExp, Environment(Map[String, Type](), Map[String, Type]()), UnknownType())
  }

  def eval(exp: Exp, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"eval: ${exp.token}")
    exp match {
      case Lit(_, _) => exp
      case m@Match(_, _, _) => evalMatch(m, env, expectedType)
      case alias@Alias(_, _, _, _) => evalAlias(alias, env, expectedType)
      case adt@Adt(_, _, _, _, _, _) => evalAdt(adt, env, expectedType)
      case let@Let(_, _, _, _, _, _) => evalLet(let, env, expectedType)
      case prim@Prim(_, _, _, _) => evalPrim(prim, env, expectedType)
      case ref@Ref(_, _) =>
        ref.usingType(typeConforms(ref.token, getName(ref.token, env, ref.ident), expectedType, env))
      case branch@Branch(_, _, _, _) => evalBranch(branch, env, expectedType)
      case list@ListDef(_, _) => evalListDef(list, env, expectedType)
      case array@ArrayDef(_, _) => evalArrayDef(array, env, expectedType)
      case set@SetDef(_, _) => evalSetDef(set, env, expectedType)
      case tuple@TupleDef(_, _) => evalTupleDef(tuple, env, expectedType)
      case dict@DictDef(_, _) => evalDictDef(dict, env, expectedType)
      case NoOp(_) => exp
      case _ =>
        reportUnexpected(exp.token)
        exp
    }
  }

  def evalMatch(m: Match, env: SemanticAnalyzer.Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalMatch: ${m.value.token.tokenText}")
    val valueType = eval(m.value, env, UnknownType())
    // TODO
    ???
  }

  def evalAlias(alias: Alias, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"addAlias: ${alias.toString}")
    val afterAlias = eval(alias.afterAlias, addAlias(env, alias.alias, alias.actualType), expectedType)
    Alias(alias.token, alias.alias, alias.actualType, afterAlias).usingType(afterAlias.expType)
  }

  def evalAdt(adt: Adt, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalAdt: ${adt.ident}")
    ???
  }

  def evalLet(let: Let, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalLet: ${let.ident}")
    val letValue = typeCheck(let.expValue, env, let.letType)
    val body = typeCheck(let.afterLet, addName(env, let.ident, letValue.expType), expectedType)
    Let(let.token, let.isLazy, let.ident, letValue.expType, letValue, body).usingType(body.expType)
  }

  def evalPrim(prim: Prim, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalPrim: ${prim.op.toString}")
    val left = typeCheck(prim.left, env, UnknownType()) // TODO: FIX TYPE PASSED IN?
    val right = typeCheck(prim.right, env, left.expType)
    val opType = typesConformToOperator(prim.token, prim.op, left.expType, right.expType, env)
    val primType = typeConforms(prim.token, opType, expectedType, env)
    Prim(prim.token, prim.op, left, right).usingType(primType)
  }

  def evalBranch(branch: Branch, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalBranch: ${branch.token.tokenText}")
    val condition = typeCheck(branch.condition, env, BoolType())
    val elseBranch = typeCheck(branch.elseBranch, env, expectedType)
    val ifBranch = typeCheck(branch.ifBranch, env, elseBranch.expType)
    Branch(branch.token, condition, ifBranch, elseBranch).usingType(elseBranch.expType)
  }

  def evalListDef(listDef: ListDef, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalListDef: ${listDef.token.tokenText}")
    val typedListValues = listDef.values.map(eval(_, env, listDef.expType.asInstanceOf[ListType].listType))
    val listExpType = if (typedListValues.isEmpty) UnknownType() else typedListValues.head.expType
    val listType = typeConforms(listDef.token, ListType(listExpType), expectedType, env)
    ListDef(listDef.token, typedListValues).usingType(listType)
  }

  def evalArrayDef(arrayDef: ArrayDef, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalArrayDef: ${arrayDef.token.tokenText}")
    val typedArrayValues = arrayDef.values.map(eval(_, env, arrayDef.expType.asInstanceOf[ArrayType].arrayType))
    val arrayExpType = if (typedArrayValues.isEmpty) UnknownType() else typedArrayValues.head.expType
    val arrayType = typeConforms(arrayDef.token, ArrayType(arrayExpType), expectedType, env)
    ArrayDef(arrayDef.token, typedArrayValues).usingType(arrayType)
  }

  def evalSetDef(setDef: SetDef, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalSetDef: ${setDef.token.tokenText}")
    val typedSetValues = setDef.values.map(eval(_, env, setDef.expType.asInstanceOf[SetType].setType))
    val setExpType = if (typedSetValues.isEmpty) UnknownType() else typedSetValues.head.expType
    val setType = typeConforms(setDef.token, ArrayType(setExpType), expectedType, env)
    SetDef(setDef.token, typedSetValues).usingType(setType)
  }

  def evalTupleDef(tupleDef: TupleDef, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalTupleDef: ${tupleDef.token.tokenText}")
    val tupleType = typeConforms(tupleDef.token, tupleDef.expType, expectedType, env).asInstanceOf[TupleType]
    val tupleTypes = tupleDef.values.map(_.expType).
                            zip(tupleType.tupleTypes).
                            map(t => typeConforms(tupleDef.token, t._1, t._2, env))
    val typedValues = tupleDef.values.zip(tupleTypes).map(v => v._1.usingType(v._2))
    TupleDef(tupleDef.token, typedValues).usingType(tupleType)
  }

  def evalDictDef(dictDef: DictDef, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalDictDef: ${dictDef.token.tokenText}")
    val dictType = typeConforms(dictDef.token, dictDef.expType, expectedType, env).asInstanceOf[DictType]
    val typedDictValues = dictDef.mapping.map(m =>
                            Parser.Map(
                              m.key.usingType(typeConforms(m.key.token, m.key.expType, dictType.keyType, env)),
                              m.value.usingType(typeConforms(m.value.token, m.value.expType, dictType.valueType, env))
                            ))
    DictDef(dictDef.token, typedDictValues).usingType(dictType)
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
