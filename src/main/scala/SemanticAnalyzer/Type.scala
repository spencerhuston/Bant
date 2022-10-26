package SemanticAnalyzer

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

abstract class Type {
  def printType(): String
}

object TypeUtil {
  def printListType(types: ArrayBuffer[Type]): String = {
    types.tail.foldLeft(types.head.printType())(_ + "," + _.printType)
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
case class AdtType(ident: String,
                   generics: ArrayBuffer[Type],
                   fieldNames: ArrayBuffer[String]) extends Type {
  override def printType(): String = {
    s"<$ident[${TypeUtil.printListType(generics)}]" +
      s"(${fieldNames.tail.foldLeft(fieldNames.head)(_ + "," + _)})>"
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