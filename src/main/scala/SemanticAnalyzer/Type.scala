package SemanticAnalyzer

import Parser.{Exp, Generic, Signature}
import SemanticAnalyzer.Environment

import scala.collection.mutable.ArrayBuffer

abstract class Type {
  def printType(): String
}

object TypeUtil {
  def printListType(types: ArrayBuffer[Type]): String = {
    types.map(_.printType()).mkString(",")
  }

  def isLiteralOrCollectionType(t: Type): Boolean = {
    t match {
      case IntType() | BoolType() | CharType() | StringType() | NullType() => true
      case ListType(lt) => isLiteralOrCollectionType(lt)
      case ArrayType(at) => isLiteralOrCollectionType(at)
      case SetType(st) => isLiteralOrCollectionType(st)
      case TupleType(lts) => lts.forall(isLiteralOrCollectionType)
      case DictType(kt, vt) =>
        isLiteralOrCollectionType(kt) && isLiteralOrCollectionType(vt)
      case _ => false
    }
  }

  def genericToType(g: Generic): GenericType = GenericType(g.ident, g.lowerBound, g.upperBound)
}

// Primitives
case class IntType() extends Type {
  override def printType(): String = "<int>"
}
case class BoolType() extends Type {
  override def printType(): String = "<bool>"
}
case class CharType() extends Type {
  override def printType(): String = "<char>"
}
case class StringType() extends Type {
  override def printType(): String = "<string>"
}
case class NullType() extends Type {
  override def printType(): String = "<null>"
}

// Collections
case class ListType(listType: Type) extends Type {
  override def printType(): String = s"<List[${listType.printType()}]>"
}
case class ArrayType(arrayType: Type) extends Type {
  override def printType(): String = s"<Array[${arrayType.printType()}]>"
}
case class SetType(setType: Type) extends Type {
  override def printType(): String = s"<Set[${setType.printType()}]>"
}
case class TupleType(tupleTypes: ArrayBuffer[Type]) extends Type {
  override def printType(): String = {
    s"<Tuple[${TypeUtil.printListType(tupleTypes)}]>"
  }
}
case class DictType(keyType: Type,
                    valueType: Type) extends Type {
  override def printType(): String = {
    s"<Dict[${keyType.printType()}, ${valueType.printType()}]>"
  }
}

// Fancy
case class UnknownRefType(ident: String,
                          generics: ArrayBuffer[Type],
                          fieldNames: ArrayBuffer[String]) extends Type {
  override def printType(): String = {
    s"<ref? $ident" +
      (if (generics.nonEmpty) s"[${TypeUtil.printListType(generics)}]" else "") +
      (if (fieldNames.nonEmpty) s"(${fieldNames.mkString(",")})>" else ">")
  }
}
case class GenericType(ident: String, lowerBound: String, upperBound: String) extends Type {
  override def printType(): String = {
    ident +
      (if (lowerBound.nonEmpty) s" :> $lowerBound" else "") +
      (if (upperBound.nonEmpty) s" <: $upperBound" else "")
  }
}
case class ConstructorType(memberTypes: ArrayBuffer[Type])
case class AdtType(instantiated: Boolean,
                   ident: String,
                   generics: ArrayBuffer[GenericType],
                   constructorTypes: ArrayBuffer[ConstructorType]) extends Type {
  override def printType(): String = {
    s"<type $ident" +
      (if (instantiated) " inst! " else "")
      (if (generics.nonEmpty) s"[${generics.map(_.printType()).mkString(",")}]" else "") +
      (if (constructorTypes.nonEmpty)
        s"(${constructorTypes.map(c =>
          c.memberTypes.map(_.printType())
            .mkString(","))
          .mkString("|")})"
      else "") + ">"
  }
}
case class RecordType(instantiated: Boolean,
                      ident: String,
                      isSealed: Boolean,
                      superType: String,
                      generics: ArrayBuffer[GenericType],
                      fields: Map[String, Type]) extends Type {
  override def printType(): String = {
    s"<record $ident" +
      (if (generics.isEmpty) "" else s"[${generics.map(_.printType()).mkString(",")}]") +
      (if (superType == "$None$") "" else s" => $superType") +
      (if (fields.isEmpty) "" else s"(${fields.map(m => m._1 + "::" + m._2.printType()).mkString(",")})") +
      ">"
  }
}
case class TypeclassRef(isSealed: Boolean,
                        ident: String,
                        parameter: GenericType,
                        superclass: String,
                        signatures: ArrayBuffer[Signature]) extends Type {
  override def printType(): String = {
    s"<" + (if (isSealed) "sealed" else "") + s"typeclass $ident" +
      parameter.printType() +
      (if (superclass.isEmpty) "" else s" => $superclass") +
      (if (signatures.isEmpty) "" else s"(${signatures.map(s => s.name + "=" + s.funcType.printType()).mkString(",")})") +
      ">"
  }
}
case class FuncType(generics: ArrayBuffer[GenericType],
                    argTypes: ArrayBuffer[Type],
                    returnType: Type,
                    env: Environment,
                    body: Exp) extends Type {
  override def printType(): String = {
    s"<func " +
      (if (generics.isEmpty) "" else s"[${generics.map(_.printType()).mkString(",")}] ") +
      (if (argTypes.isEmpty) "() " else s"(${TypeUtil.printListType(argTypes)}) ") +
      s"-> ${returnType.printType()}>"
  }
}

// Type-Checking
case class UnknownType() extends Type {
  override def printType(): String = "<unknown>"
}