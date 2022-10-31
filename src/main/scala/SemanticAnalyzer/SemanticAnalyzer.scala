package SemanticAnalyzer

import Lexer.SyntaxDefinitions.Delimiters
import Lexer.SyntaxDefinitions.Delimiters.{arithTypesNotPlus, getValue, logicTypes, looseComparisonTypes, plusTypes, strictComparisonTypes}
import Lexer.Token
import Logger.Level.DEBUG
import Logger.Logger.{ERROR, LOG, WARN, lineList}
import Parser.{Adt, Alias, ArrayDef, Branch, DictDef, Exp, FuncDef, Generic, Let, ListDef, Lit, Match, Member, NoOp, Prim, Record, Ref, SetDef, Signature, TupleDef, Typeclass}

import scala.collection.mutable.ArrayBuffer

object SemanticAnalyzer {
  var numErrors = 0
  var warnings = 0

  case class Environment(map: Map[String, Type],
                         aliases: Map[String, Type],
                         typeclasses: Map[String, Type])

  def addName(env: Environment, name: String, newType: Type): Environment = {
    Environment(env.map + (name -> newType), env.aliases, env.typeclasses)
  }

  def getName(token: Token, env: Environment, name: String): Type = {
    env.map.get(name) match {
      case s@Some(_) => s.value
      case _ =>
        reportNoSuchName(token, name)
        UnknownType()
    }
  }

  def addGenericsToEnv(generics: ArrayBuffer[GenericType], env: Environment): Environment = {
    Environment(env.map ++ generics.map(g => g.ident -> g).toMap, env.aliases, env.typeclasses)
  }

  def addAlias(env: Environment, name: String, newType: Type): Environment = {
    Environment(env.map, env.aliases + (name -> newType), env.typeclasses)
  }

  def getAlias(token: Token, env: Environment, name: String): Type = {
    env.aliases.get(name) match {
      case s@Some(_) => s.value
      case _ =>
        reportNoSuchName(token, name)
        UnknownType()
    }
  }

  def addTypeclass(env: Environment, name: String, newType: TypeclassRef): Environment = {
    Environment(env.map, env.aliases, env.typeclasses + (name -> newType))
  }

  def getTypeclass(token: Token, env: Environment, name: String): Type = {
    env.typeclasses.get(name) match {
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
      case AdtType(_, _, _) => ??? // TODO: Could be ADT or Record
      //case FuncType(_, _) => ??? // TODO
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
      case (AdtType(i1, g1, f1), AdtType(i2, g2, f2))
        if (i1 == i2) && (g1.length == g2.length) && (f1.length == f2.length) &&
          f1.zip(f2).count(f => f._1 != f._2) == 0 =>
        AdtType(i1, g1, f1) // TODO: FOR ADT APP
      case (AdtType(i, _, _), t2) => typeConforms(token, getAlias(token, env, i), t2, env)
      case (t1, AdtType(i, _, _)) => typeConforms(token, t1, getAlias(token, env, i), env)
      //case (FuncType(args1, rt1), FuncType(args2, rt2)) if args1.length == args2.length =>
        //FuncType(listTypeConforms(args1, args2), typeConforms(token, rt1, rt2, env))
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

  def checkDerivable(token: Token, typeclassIdent: String, env: Environment): Unit = {
    if (typeclassIdent.nonEmpty) {
      env.typeclasses.get(typeclassIdent) match {
        case Some(_) =>
        case _ =>
          ERROR(s"Error: <typeclass> $typeclassIdent does not exist in this scope")
          reportLine(token)
      }
    }
  }

  def checkRecordExtendable(token: Token, superIdent: String, env: Environment): Unit = {
    if (superIdent.nonEmpty) {
      env.map.get(superIdent) match {
        case Some(superRecord) =>
          if (!superRecord.isInstanceOf[RecordType]) {
            ERROR(s"Error: type $superIdent is not a record type")
            reportLine(token)
          }
          else if (superRecord.asInstanceOf[RecordType].isSealed) {
            ERROR(s"Error: <record> $superIdent is sealed, cannot be extended")
            reportLine(token)
          }
        case _ =>
          ERROR(s"Error: <record> $superIdent does not exist in this scope")
          reportLine(token)
      }
    }
  }

  def checkTypeclassExtendable(token: Token, superIdent: String, env: Environment): Unit = {
    if (superIdent.nonEmpty) {
      env.typeclasses.get(superIdent) match {
        case Some(superclass) =>
          if (!superclass.isInstanceOf[TypeclassRef]) {
            ERROR(s"Error: $superIdent is not a typeclass")
            reportLine(token)
          }
          else if (superclass.asInstanceOf[TypeclassRef].isSealed) {
            ERROR(s"Error: <typeclass> $superIdent is sealed, cannot be extended")
            reportLine(token)
          }
        case _ =>
          ERROR(s"Error: <typeclass> $superIdent does not exist in this scope")
          reportLine(token)
      }
    }
  }

  def typeCheckGenericParams(exp: Exp, env: Environment): ArrayBuffer[GenericType] = {
    val generics: ArrayBuffer[Generic] = exp match {
      case a@Adt(_, _, _, _, _, _) => a.generics
      case r@Record(_, _, _, _, _, _, _, _) => r.generics
      case t@Typeclass(_, _, _, _, _, _, _) => t.generics
      case f@FuncDef(_, _, _, _, _, _) => f.generics
      case _ =>
        ERROR(s"Error: Type does not allow generics")
        reportLine(exp.token)
        return ArrayBuffer[GenericType]()
    }

    def genericToType(g: Generic): GenericType = GenericType(g.ident, g.lowerBound, g.upperBound)

    def checkBounds(g: Generic): Unit = {
      def reportNotRecordType(bound: String): Unit = {
        ERROR(s"Error: Bound $bound is not a <record> type")
        reportLine(exp.token)
      }

      env.map.get(g.lowerBound) match {
        case Some(lower) =>
          if (!lower.isInstanceOf[RecordType])
            reportNotRecordType(g.lowerBound)
        case _ =>
      }
      env.map.get(g.upperBound) match {
        case Some(upper) =>
          if (!upper.isInstanceOf[RecordType])
            reportNotRecordType(g.upperBound)
          else if (upper.asInstanceOf[RecordType].isSealed) {
            ERROR(s"Error: <record> ${g.upperBound} is sealed, cannot have a lower-bound")
            reportLine(exp.token)
          }
        case _ =>
      }
    }

    // 1. For each generic check if the ident exists in env*
    // 1.a If it doesn't its a generic placeholder name, put as GenericType in env*
    // 1.b If it does it's another ADT or a Record or self-reference
    // 1.b.a If it's an ADT make sure it doesn't have lower and upper bounds
    // 1.b.b If it's a Record check: it is not sealed if there is a lower type, and upper type is not sealed
    // 1.b.c If its a self-reference, follow ADT rules
    // 2. Convert generics to generic types, making sure of all checks
    // 3. Re-add self-type into env with generics added
    generics.map(g => env.map.get(g.ident) match { // 1.
      case Some(typeVal) => // 1.b
        typeVal match {
          case AdtType(_, _, _) =>
            if (g.lowerBound.nonEmpty || g.upperBound.nonEmpty) { // 1.b.a
              ERROR(s"Error: <type> $g can not be type-bound")
              reportLine(exp.token)
            }
            genericToType(g)
          case RecordType(recordIdent, isSealed, _, _, _) => // 1.b.b
            if (isSealed && g.lowerBound.nonEmpty) {
              ERROR(s"Error: <record> $recordIdent is sealed, cannot have a lower-bound")
              reportLine(exp.token)
            }
            checkBounds(g)
            genericToType(g)
        }
      case _ => // 1.a
        checkBounds(g)
        genericToType(g)
    })
  }

  def deduceUnknownRefType(token: Token, unknown: UnknownRefType, env: Environment): Type = {
    // check all types are UnknownRefTypes in UnknownRefType generics
    // check all UnknownRefTypes that arent refs from env are found in genericTypes
    def genericsAreUnknownRef(generics: ArrayBuffer[Type]): Boolean = {
      if (generics.nonEmpty)
        generics.forall(g => g.isInstanceOf[UnknownRefType] && genericsAreUnknownRef(g.asInstanceOf[UnknownRefType].generics))
      else
        true
    }

    def genericTypesAreDefined(generics: ArrayBuffer[Type]): Boolean = {
      generics.forall(g => {
        val unknownType = g.asInstanceOf[UnknownRefType]
        env.map.get(unknownType.ident) match {
          case Some(_) =>
            genericTypesAreDefined(unknownType.generics)
          case _ =>
            ERROR(s"Error: Generic parameter \"${unknownType.ident}\" not defined")
            reportLine(token)
            false
        }
      })
    }

    if (!genericsAreUnknownRef(unknown.generics)) {
      ERROR(s"Error: Literal types disallowed in generic parameter definition")
      reportLine(token)
      unknown
    }
    else if (!genericTypesAreDefined(unknown.generics))
      unknown
    else {
      env.map.get(unknown.ident) match {
        case Some(ref) =>
          ref match {
            case knownAdt@AdtType(_, adtG, _) =>
              if (unknown.generics.length == adtG.length)
                knownAdt
              else {
                ERROR(s"Error: Generic parameters for <type> ${unknown.ident} do not match reference")
                reportLine(token)
                unknown
              }
            case knownRecord@RecordType(_, _, _, recordG, _) =>
              if (unknown.generics.length == recordG.length)
                knownRecord
              else {
                ERROR(s"Error: Generic parameters for <record> ${unknown.ident} do not match reference")
                reportLine(token)
                unknown
              }
            case generic@GenericType(_, _, _) =>
              generic
            case _ =>
              ERROR(s"Error: Invalid reference to type \"${unknown.ident}\"")
              reportLine(token)
              unknown
          }
        case _ =>
          reportNoSuchName(token, s"${unknown.ident}")
          unknown
      }
    }
  }

  def eval(rootExp: Exp): Exp = {
    LOG(DEBUG, s"eval: ${rootExp.token}")
    eval(rootExp, Environment(Map[String, Type](), Map[String, Type](), Map[String, Type]()), UnknownType())
  }

  def eval(exp: Exp, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"eval: ${exp.token}")
    exp match {
      case Lit(_, _) => exp
      case m@Match(_, _, _) => evalMatch(m, env, expectedType)
      case alias@Alias(_, _, _, _) => evalAlias(alias, env, expectedType)
      case adt@Adt(_, _, _, _, _, _) => evalAdt(adt, env, expectedType)
      case record@Record(_, _, _, _, _, _, _, _) => evalRecord(record, env, expectedType)
      case typeclass@Typeclass(_, _, _, _, _, _, _) => evalTypeclass(typeclass, env, expectedType)
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
    val adtEnvNoGenerics = addName(env, adt.ident, UnknownType())
    checkDerivable(adt.token, adt.derivedFrom, env)
    val genericTypes: ArrayBuffer[GenericType] = typeCheckGenericParams(adt, adtEnvNoGenerics)
    val genericEnv = addGenericsToEnv(genericTypes, adtEnvNoGenerics)
    val adtEnv = addName(genericEnv, adt.ident, AdtType(adt.ident, genericTypes, ArrayBuffer[ConstructorType]()))
    val constructorTypes: ArrayBuffer[ConstructorType] = adt.constructors.map(ct =>
      ConstructorType(ct.members.map {
        case unknown@UnknownRefType(_, _, f) =>
          if (f.nonEmpty) {
            ERROR(s"Error: Cannot deconstruct type in <type> definition")
            reportLine(adt.token)
            unknown
          }
          else
            deduceUnknownRefType(adt.token, unknown, adtEnv)
        case mt: Type => mt
      })
    )
    val adtType = AdtType(adt.ident, genericTypes, constructorTypes)
    val afterAdt = eval(adt.afterAdt, addName(env, adt.ident, adtType), expectedType)
    Adt(adt.token, adt.ident, adt.generics, adt.derivedFrom, adt.constructors, afterAdt).usingType(adtType)
  }

  def evalRecord(record: Record, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalRecord: ${record.ident}")
    val recordEnvNoGenerics = addName(env, record.ident, UnknownType())
    checkDerivable(record.token, record.derivedFrom, env)
    val genericTypes: ArrayBuffer[GenericType] = typeCheckGenericParams(record, recordEnvNoGenerics)
    val genericEnv = addGenericsToEnv(genericTypes, recordEnvNoGenerics)
    checkRecordExtendable(record.token, record.superType, env)
    val recordEnv = addName(genericEnv, record.ident, RecordType(record.ident, record.isSealed, record.superType, genericTypes, Map[String, Type]()))
    val fields: Map[String, Type] = record.members.map {
      case Member(ident, unknownRefType: UnknownRefType) =>
        ident -> deduceUnknownRefType(record.token, unknownRefType, recordEnv)
      case m => m.ident -> m.memberType
    }.toMap
    val recordType = RecordType(record.ident, record.isSealed, record.superType, genericTypes, fields)
    val afterRecord = eval(record.afterRecord, addName(env, record.ident, recordType), expectedType)
    Record(record.token,
      record.isSealed,
      record.ident,
      record.generics,
      record.superType,
      record.derivedFrom,
      record.members,
      afterRecord).usingType(recordType)
  }

  def evalTypeclass(typeclass: Typeclass, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalTypeclass: ${typeclass.ident}")
    val genericTypes: ArrayBuffer[GenericType] = typeCheckGenericParams(typeclass, env)
    val genericEnv = addGenericsToEnv(genericTypes, env)
    checkTypeclassExtendable(typeclass.token, typeclass.superclass, env)
    val signatures: ArrayBuffer[Signature] = typeclass.signatures.map(s => {
      Signature(s.name, {
        if (!s.funcType.isInstanceOf[FuncType]) {
          ERROR(s"Error: Typeclass signature ${s.name} requires function type, not ${s.funcType.printType()}")
          reportLine(typeclass.token)
          s.funcType
        }
        else {
          val funcType = s.funcType.asInstanceOf[FuncType]
          def deduceArgType(argType: Type): Type = {
            argType match {
              case unknown@UnknownRefType(_, _, f) =>
                if (f.nonEmpty) {
                  ERROR(s"Error: Cannot deconstruct type in <typeclass> signature definition")
                  reportLine(typeclass.token)
                  unknown
                }
                else
                  deduceUnknownRefType(typeclass.token, unknown, genericEnv)
              case t: Type => t
            }
          }
          FuncType(funcType.generics, funcType.argTypes.map { deduceArgType }, deduceArgType(funcType.returnType))
        }
      })
    })
    val typeclassRef = TypeclassRef(typeclass.isSealed, typeclass.ident, genericTypes, typeclass.superclass, signatures)
    val afterTypeclass = eval(typeclass.afterTypeclass, addTypeclass(env, typeclass.ident, typeclassRef), expectedType)
    Typeclass(typeclass.token,
      typeclass.isSealed,
      typeclass.ident,
      typeclass.generics,
      typeclass.superclass,
      signatures,
      afterTypeclass)
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
