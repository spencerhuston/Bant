package SemanticAnalyzer

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
    s"<ref $ident" +
      s"[${TypeUtil.printListType(generics)}]" +
      s"(${fieldNames.mkString(",")})>"
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
case class AdtType(ident: String,
                   generics: ArrayBuffer[GenericType],
                   constructorTypes: ArrayBuffer[ConstructorType]) extends Type {
  override def printType(): String = {
    s"<type $ident" +
      (if (generics.nonEmpty) s"[${generics.map(_.printType()).mkString(",")}]" else "") +
      (if (constructorTypes.nonEmpty)
        s"(${constructorTypes.map(c =>
          c.memberTypes.map(_.printType())
            .mkString(","))
          .mkString("|")})"
      else "") + ">"

  }
}
case class RecordType(ident: String,
                      isSealed: Boolean,
                      superType: String,
                      generics: ArrayBuffer[GenericType],
                      fieldNames: ArrayBuffer[String]) extends Type {
  override def printType(): String = {
    s"<record $ident" +
      (if (superType.isEmpty) "" else s"=> $superType") +
      s"[${generics.map(_.printType()).mkString(",")}]" +
      s"(${fieldNames.mkString(",")})>"
  }
}
case class FuncType(argTypes: ArrayBuffer[Type],
                    returnType: Type) extends Type {
  override def printType(): String = {
    s"<Func(${TypeUtil.printListType(argTypes)}) -> $returnType>"
  }
}

// Type-Checking
case class UnknownType() extends Type {
  override def printType(): String = "<unknown>"
}