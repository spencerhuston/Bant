package SemanticAnalyzer

import Lexer.SyntaxDefinitions.Delimiters
import Lexer.SyntaxDefinitions.Delimiters.{arithTypesNotPlus, getValue, logicTypes, looseComparisonTypes, plusTypes, strictComparisonTypes}
import Lexer.Token
import Logger.Level.DEBUG
import Logger.Logger.{ERROR, LOG, WARN, lineList}
import Parser.Parser.none
import Parser.{Adt, Alias, ArrayDef, Branch, DictDef, Exp, FuncApp, FuncDef, Generic, Instance, IntVal, Let, ListDef, Lit, Match, Member, NoOp, Parameter, Prim, Prog, Record, RecordAccess, Ref, SetDef, Signature, TupleAccess, TupleDef, Typeclass}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object SemanticAnalyzer {
  var numErrors = 0
  var warnings = 0

  case class Environment(map: Map[String, Type],
                         aliases: Map[String, Type],
                         typeclasses: Map[String, Type])

  def emptyEnv: Environment = {
    Environment(Map[String, Type](),
      Map[String, Type](),
      Map[String, Type]())
  }

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

  def addParamsToEnv(params: ArrayBuffer[Parameter], env: Environment): Map[String, Type] = {
    val paramMap: ArrayBuffer[Map[String, Type]] = params.map(p => {
      val paramType = deduceParamType(p.token, p.paramType, env)
      paramType match {
        case adt@AdtType(ident, g, constructorTypes) =>
          val adtMap: Map[String, Type] = Map[String, Type](ident -> adt)
          val unknownRefType = p.paramType.asInstanceOf[UnknownRefType]
          if (g.nonEmpty) {
            ERROR(s"Error: Generic constructor parameters disallowed in function parameter definition")
            reportLine(p.token)
          }
          val fieldTypes: Map[String, Type] = constructorTypes.find(c => c.memberTypes.length == unknownRefType.fieldNames.length) match {
            case Some(ct) =>
              unknownRefType.fieldNames.zip(ct.memberTypes).map(t => t._1 -> t._2).toMap
            case _ =>
              if (unknownRefType.fieldNames.nonEmpty) {
                ERROR(s"Error: Constructor does not match for <type> $ident")
                reportLine(p.token)
              }
              Map[String, Type]()
          }
          adtMap ++ fieldTypes
        case record@RecordType(ident, _, _, g, fields) =>
          val recordMap: Map[String, Type] = Map[String, Type](ident -> record)
          val unknownRefType = p.paramType.asInstanceOf[UnknownRefType]
          if (g.nonEmpty) {
            ERROR(s"Error: Generic constructor parameters disallowed in function parameter definition")
            reportLine(p.token)
          }
          else if (unknownRefType.fieldNames.nonEmpty && unknownRefType.fieldNames.length != fields.toList.length) {
            ERROR(s"Error: Constructor does not match for <record> $ident")
            reportLine(p.token)
          }
          recordMap ++ unknownRefType.fieldNames.zip(fields).map(t => t._1 -> t._2._2).toMap
        case _ =>
          Map[String, Type](p.ident -> p.paramType)
      }
    })
    env.map ++ paramMap.foldLeft(Map[String, Type]())(_ ++ _)
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

  def getSuperclassSignatures(typeclass: Typeclass, env: Environment): ArrayBuffer[Signature] = {
    if (isTypeclassExtendable(typeclass.token, typeclass.superclass, env)) {
      typeclass.signatures ++
        getSuperclassSignatures(typeclass.token,
          getTypeclass(typeclass.token, env, typeclass.superclass).asInstanceOf[TypeclassRef],
          env)
    }
    else
      typeclass.signatures
  }

  def getSuperclassSignatures(token: Token, typeclass: TypeclassRef, env: Environment): ArrayBuffer[Signature] = {
    if (isTypeclassExtendable(token, typeclass.superclass, env)) {
      typeclass.signatures ++
        getSuperclassSignatures(token, getTypeclass(token, env, typeclass.superclass).asInstanceOf[TypeclassRef], env)
    }
    else
      typeclass.signatures
  }

  def typeConforms(token: Token, expressionType: Type, expectedType: Type, env: Environment): Type = {
    LOG(DEBUG, s"typeConforms")

    def typeWellFormed(t: Type): Type = t match {
      case ListType(lt) => ListType(typeWellFormed(lt))
      case ArrayType(at) => ArrayType(typeWellFormed(at))
      case SetType(st) => SetType(typeWellFormed(st))
      case TupleType(tts) => TupleType(tts.map(typeWellFormed))
      case DictType(kt, vt) => DictType(typeWellFormed(kt), typeWellFormed(vt))
      case AdtType(i, g, cs) => AdtType(i, g, cs.map(ct => ConstructorType(ct.memberTypes.map(typeWellFormed))))
      case RecordType(_, _, _, _, _) => ??? // TODO
      case FuncType(_, _, _, _, _) => ??? // TODO
      case UnknownRefType(_, _, _) => ??? // TODO
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
      case (AdtType(i1, g1, f1), AdtType(_, g2, f2))
        if (g1.length == g2.length) && (f1.length == f2.length) =>
        val constructorTypes = f1.zip(f2)
          .map(cs => ConstructorType(cs._1.memberTypes.zip(cs._2.memberTypes)
            .map(cts => typeConforms(token, cts._1, cts._2, env))))
        AdtType(i1, g1, constructorTypes)
      case (UnknownRefType(i, _, _), t2) => typeConforms(token, getAlias(token, env, i), t2, env)
      case (t1, UnknownRefType(i, _, _)) => typeConforms(token, t1, getAlias(token, env, i), env)
      //case (FuncType(gen1, args1, rt1), FuncType(gen2, args2, rt2)) if args1.length == args2.length =>
        // TODO for optional generic parameters at function call site
        //FuncType(, listTypeConforms(args1, args2), typeConforms(token, rt1, rt2, env))
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

  def isRecordExtendable(token: Token, superIdent: String, env: Environment): Boolean = {
    if (superIdent.nonEmpty) {
      env.map.get(superIdent) match {
        case Some(superRecord) =>
          if (!superRecord.isInstanceOf[RecordType]) {
            ERROR(s"Error: type $superIdent is not a record type")
            reportLine(token)
            false
          }
          else if (superRecord.asInstanceOf[RecordType].isSealed) {
            ERROR(s"Error: <record> $superIdent is sealed, cannot be extended")
            reportLine(token)
            false
          }
          else true
        case _ =>
          ERROR(s"Error: <record> $superIdent does not exist in this scope")
          reportLine(token)
          false
      }
    }
    else false
  }

  def isTypeclassExtendable(token: Token, superIdent: String, env: Environment): Boolean = {
    if (superIdent.nonEmpty) {
      env.typeclasses.get(superIdent) match {
        case Some(superclass) =>
          if (!superclass.isInstanceOf[TypeclassRef]) {
            ERROR(s"Error: $superIdent is not a typeclass")
            reportLine(token)
            false
          }
          else if (superclass.asInstanceOf[TypeclassRef].isSealed) {
            ERROR(s"Error: <typeclass> $superIdent is sealed, cannot be extended")
            reportLine(token)
            false
          }
          else true
        case _ =>
          ERROR(s"Error: <typeclass> $superIdent does not exist in this scope")
          reportLine(token)
          false
      }
    }
    else false
  }

  def typeCheckGenericParams(exp: Exp, env: Environment): ArrayBuffer[GenericType] = {
    val generics: ArrayBuffer[Generic] = exp match {
      case a@Adt(_, _, _, _, _, _) => a.generics
      case r@Record(_, _, _, _, _, _, _, _) => r.generics
      case t@Typeclass(_, _, _, _, _, _, _) => ArrayBuffer[Generic](t.parameter)
      case f@FuncDef(_, _, _, _, _, _) => f.generics
      case _ =>
        ERROR(s"Error: Type does not allow generics")
        reportLine(exp.token)
        return ArrayBuffer[GenericType]()
    }

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

    generics.map(g => env.map.get(g.ident) match { // 1.
      case Some(typeVal) => // 1.b
        typeVal match {
          case AdtType(_, _, _) =>
            if (g.lowerBound.nonEmpty || g.upperBound.nonEmpty) { // 1.b.a
              ERROR(s"Error: <type> $g can not be type-bound")
              reportLine(exp.token)
            }
            TypeUtil.genericToType(g)
          case RecordType(recordIdent, isSealed, _, _, _) => // 1.b.b
            if (isSealed && g.lowerBound.nonEmpty) {
              ERROR(s"Error: <record> $recordIdent is sealed, cannot have a lower-bound")
              reportLine(exp.token)
            }
            checkBounds(g)
            TypeUtil.genericToType(g)
        }
      case _ => // 1.a
        checkBounds(g)
        TypeUtil.genericToType(g)
    })
  }

  def deduceParamType(token: Token, t: Type, env: Environment): Type = {
    t match {
      case unknown@UnknownRefType(_, _, _) => deduceUnknownRefType(token, unknown, env)
      case GenericType(ident, _, _) => getName(token, env, ident)
      case _ => t
    }
  }

  def typeCheckDefaultParams(params: ArrayBuffer[Parameter]): Unit = {
    params.takeRight(params.zip(List.range(0, params.length)).find(p => p._1.default match {
      case NoOp(_) => false
      case _ => true
    }) match {
      case Some(funcIndex) => params.length - funcIndex._2
      case _ => 0
    }).foreach(p => p.default match {
      case NoOp(_) =>
        ERROR(s"Error: Ambiguous function parameter(s).\n" +
          s"${" " * "Error: ".length}Function parameter \"${p.ident}\" requires default value or position shifted to before default parameters")
        reportLine(p.token)
      case _ =>
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
      env.aliases.get(unknown.ident) match {
        case Some(aliasType) => aliasType
        case _ =>
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
  }

  def eval(rootExp: Exp): Exp = {
    LOG(DEBUG, s"eval: ${rootExp.token}")
    eval(rootExp, Environment(Map[String, Type](), Map[String, Type](), Map[String, Type]()), UnknownType())
  }

  def eval(exp: Exp, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"eval: ${exp.token}")
    exp match {
      case Lit(_, _) => exp.usingType(typeConforms(exp.token, exp.expType, expectedType, env))
      case m@Match(_, _, _) => evalMatch(m, env, expectedType)
      case alias@Alias(_, _, _, _) => evalAlias(alias, env, expectedType)
      case adt@Adt(_, _, _, _, _, _) => evalAdt(adt, env, expectedType)
      case record@Record(_, _, _, _, _, _, _, _) => evalRecord(record, env, expectedType)
      case typeclass@Typeclass(_, _, _, _, _, _, _) => evalTypeclass(typeclass, env, expectedType)
      case instance@Instance(_, _, _, _, _) => evalInstance(instance, env, expectedType)
      case prog@Prog(_, _, _) => evalProg(prog, env, expectedType)
      case funcApp@FuncApp(_, _, _, _) => evalFuncApp(funcApp, env, expectedType)
      case tupleAccess@TupleAccess(_, _, _) => evalTupleAccess(tupleAccess, env, expectedType)
      case recordAccess@RecordAccess(_, _, _) => evalRecordAccess(recordAccess, env, expectedType)
      case let@Let(_, _, _, _, _, _) => evalLet(let, env, expectedType)
      case prim@Prim(_, _, _, _) => evalPrim(prim, env, expectedType)
      case ref@Ref(_, _) => ref.usingType(typeConforms(ref.token, getName(ref.token, env, ref.ident), expectedType, env))
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
    isRecordExtendable(record.token, record.superType, env)
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
    val genericTypes: GenericType = typeCheckGenericParams(typeclass, env).head
    val genericEnv = addGenericsToEnv(ArrayBuffer(genericTypes), env)
    val typeclassSignatures = getSuperclassSignatures(typeclass, genericEnv)

    val signatures: ArrayBuffer[Signature] = typeclassSignatures.map(s => {
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
          FuncType(funcType.generics, funcType.argTypes.map { deduceArgType }, deduceArgType(funcType.returnType), emptyEnv, none)
        }
      })
    })
    val typeclassRef = TypeclassRef(typeclass.isSealed, typeclass.ident, genericTypes, typeclass.superclass, signatures)
    val afterTypeclass = eval(typeclass.afterTypeclass, addTypeclass(env, typeclass.ident, typeclassRef), expectedType)
    Typeclass(typeclass.token,
      typeclass.isSealed,
      typeclass.ident,
      typeclass.parameter,
      typeclass.superclass,
      signatures,
      afterTypeclass).usingType(typeclassRef)
  }

  def evalInstance(instance: Instance, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalInstance: ${instance.instanceTypeIdent}")
    val instanceType = getName(instance.token, env, instance.instanceTypeIdent)
    if (!instanceType.isInstanceOf[AdtType] && !instanceType.isInstanceOf[RecordType]) {
      ERROR(s"Error: Invalid typeclass parameter ${instance.instanceTypeIdent}, requires <type> or <record>")
      reportLine(instance.token)
      instance
    }
    else {
      val typeclassType = getTypeclass(instance.token, env, instance.typeclassIdent)
      if (!typeclassType.isInstanceOf[TypeclassRef]) {
        ERROR(s"Error: Reference ${instance.typeclassIdent} is not a typeclass")
        reportLine(instance.token)
        instance
      }
      else {
        val typeclassRef = typeclassType.asInstanceOf[TypeclassRef]
        val genericResolvedEnv = addName(env, typeclassRef.parameter.ident, instanceType)

        typeclassRef.signatures.foreach(s => {
          val sigFuncType = s.funcType.asInstanceOf[FuncType]
          instance.funcs.find(f => {
            s.name == f.ident &&
              sigFuncType.argTypes.length == f.params.length
          }) match {
            case Some(funcDef) =>
              funcDef.params.map(_.paramType).zip(sigFuncType.argTypes)
                .foreach(t => {
                  typeConforms(funcDef.token,
                    deduceParamType(funcDef.token, t._1, genericResolvedEnv),
                    deduceParamType(funcDef.token, t._2, genericResolvedEnv),
                    genericResolvedEnv)
                })
              typeConforms(funcDef.token,
                deduceParamType(funcDef.token, funcDef.returnType, genericResolvedEnv),
                deduceParamType(funcDef.token, sigFuncType.returnType, genericResolvedEnv),
                genericResolvedEnv)
            case None =>
              ERROR(s"Error: No matching function for signature ${s.name}")
              reportLine(instance.token)
          }
        })
        instance.funcs.foreach(f => {
          val funcEnvWithGenerics = addGenericsToEnv(f.generics.map(TypeUtil.genericToType), env)
          val funcEnvWithGenericsAndParams = Environment(funcEnvWithGenerics.map ++ addParamsToEnv(f.params, funcEnvWithGenerics), env.aliases, env.typeclasses)
          typeCheckDefaultParams(f.params)
          f.params.foreach(p => p.default match {
            case NoOp(_) =>
            case _ => eval(p.default, env, p.paramType)
          })
          eval(f.body, funcEnvWithGenericsAndParams, f.returnType)
        })
        val envWithInstanceFuncs = Environment(env.map ++ instance.funcs.map(f => f.ident -> f.expType).toMap, env.aliases, env.typeclasses)
        val afterInstance = eval(instance.afterInstance, envWithInstanceFuncs, expectedType)
        Instance(instance.token,
          instance.instanceTypeIdent,
          instance.typeclassIdent,
          instance.funcs,
          afterInstance).usingType(afterInstance.expType)
      }
    }
  }

  def evalProg(prog: Prog, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalProg: ${prog.token.tokenText}")
    val funcEnv = Environment(env.map ++ prog.funcs.map(f => f.ident -> f.expType).toMap, env.aliases, env.typeclasses)
    prog.funcs.foreach(f => {
      val funcEnvWithGenerics = addGenericsToEnv(f.generics.map(TypeUtil.genericToType), funcEnv)
      val funcEnvWithGenericsAndParams = Environment(funcEnvWithGenerics.map ++ f.params.map(p => p.ident -> p.paramType).toMap, env.aliases, env.typeclasses)
      typeCheckDefaultParams(f.params)
      f.params.foreach(p => p.default match {
        case NoOp(_) =>
        case _ => eval(p.default, env, p.paramType)
      })
      eval(f.body, funcEnvWithGenericsAndParams, f.returnType)
    })
    val afterProg = eval(prog.afterProg, funcEnv, expectedType)
    Prog(prog.token, prog.funcs, afterProg).usingType(afterProg.expType)
  }

  def evalCollectionApp(argType: Type, typedIdent: Exp, funcApp: FuncApp, env: Environment): Exp = {
    LOG(DEBUG, s"evalCollectionApp: ${funcApp.token.tokenText}")
    if (funcApp.arguments.length != 1) {
      ERROR(s"Error: Collection application requires exactly 1 <int> type")
      reportLine(funcApp.token)
      funcApp
    }
    else {
      val arg = eval(funcApp.arguments.head, env, argType)
      FuncApp(funcApp.token,
        typedIdent,
        funcApp.genericParameters,
        ArrayBuffer(arg))
    }
  }

  // TODO
  def evalFuncApp(funcApp: FuncApp, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalFuncApp: ${funcApp.token.tokenText}")
    // 1. Recursively check ident until exp type is ref (unknown ref type)
    //
    // 2. If func type:
    // 2.a.a Add generic idents with matching generic parameter types
    // 2.a.b If generics are expected but not provided, add them as UnknownType and let
    //        it deduce the type from the arguments
    // 2.b Check arg length matches field count
    // 2.b.a If it doesnt check for default params. If none, throw error
    //          If they do exist, match each arg up to expected type as usual and then
    //          insert them and left over defaults into env
    // 2.b.b If it does, check each arg against the expected type
    // 2.c Add all args into func env and eval body with expected return type
    // 2.d Return the func return type
    //
    // 3. If ADT type:
    // 3.a.a Add generic idents with matching generic parameter types
    // 3.a.b If generics are expected but not provided, add them as UnknownType and let
    //        it deduce the type from the arguments
    // 3.b Get list of constructors with same field count as arg count
    // 3.c Find first one that matches arg types pair-wise
    // 3.d Return adt type with no generics and single matching constructor type
    //
    // 4. If record type:
    // 4.a.a Add generic idents with matching generic parameter types
    // 4.a.b If generics are expected but not provided, add them as UnknownType and let
    //        it deduce the type from the arguments
    // 4.b Check arg length matches field count
    // 4.c Check each type matches field types pair-wise
    // 4.c Return record type
    //
    // 5. If collection type:
    // 5.a If list/array/set:
    // 5.a.a Check argument type resolves to IntType
    // 5.a.b Return collection element type
    // 5.b.a Check argument type resolves to keyType
    // 5.b.b Return dict value type
    //
    // 6. Return type and repeat steps on parent calls until completely out of call stack
    val typedIdent = eval(funcApp.ident, env, UnknownType())
    typedIdent.expType match {
      case ListType(listType) =>
        evalCollectionApp(IntType(), typedIdent, funcApp, env).usingType(listType)
      case ArrayType(arrayType) =>
        evalCollectionApp(IntType(), typedIdent, funcApp, env).usingType(arrayType)
      case SetType(setType) =>
        evalCollectionApp(IntType(), typedIdent, funcApp, env).usingType(setType)
      case DictType(kt, vt) =>
        evalCollectionApp(kt, typedIdent, funcApp, env).usingType(vt)
      case _ =>
        ERROR(s"Error: Invalid application type")
        reportLine(funcApp.token)
        funcApp
    }
  }

  def evalTupleAccess(ta: TupleAccess, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalTupleAccess: ${ta.token}")
    val tupleType = getName(ta.token, env, ta.ref.ident)
    if (!tupleType.isInstanceOf[TupleType]) {
      ERROR(s"Error: ${ta.ref.ident} is not of type <tuple>")
      reportLine(ta.token)
      ta
    }
    else {
      @tailrec
      def tupleIndexMatches(t: TupleType, accessIndices: ArrayBuffer[IntVal], index: Int = 0): Type = {
        if (index > accessIndices.length) {
          ERROR(s"Error: More indices than nested tuples for <tuple> ${ta.ref.ident}")
          reportLine(ta.token)
          t
        }
        else if (accessIndices(index).value < 0 || accessIndices(index).value >= t.tupleTypes.length) {
          ERROR(s"Error: Invalid index for ${accessIndices(index).value} <tuple> ${ta.ref.ident}")
          reportLine(ta.token)
          t
        }
        else {
          t.tupleTypes(accessIndices(index).value) match {
            case tt@TupleType(_) => tupleIndexMatches(tt, accessIndices, index + 1)
            case otherType@_ => otherType
          }
        }
      }
      val tupleAccessType = tupleIndexMatches(tupleType.asInstanceOf[TupleType], ta.accessIndices)
      TupleAccess(ta.token,
        ta.ref,
        ta.accessIndices).usingType(typeConforms(ta.token, tupleAccessType, expectedType, env))
    }
  }

  def evalRecordAccess(ra: RecordAccess, env: Environment, expectedType: Type): Exp = {
    LOG(DEBUG, s"evalRecordAccess: ${ra.recordIdent}")
    // TODO
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
