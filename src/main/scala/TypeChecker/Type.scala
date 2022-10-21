package TypeChecker

import scala.collection.mutable.ArrayBuffer

abstract class Type { }

// Primitives
case class IntType() extends Type
case class BoolType() extends Type
case class CharType() extends Type
case class StringType() extends Type
case class NullType() extends Type

// Collections
case class ListType(listType: Type) extends Type
case class ArrayType(arrayType: Type) extends Type
case class TupleType(tupleTypes: ArrayBuffer[Type]) extends Type
case class SetType(setType: Type) extends Type
case class DictType(keyType: Type,
                    valueType: Type) extends Type

// Fancy
case class AdtType(ident: String,
                   generics: ArrayBuffer[Type],
                   fieldNames: ArrayBuffer[String]) extends Type
case class FuncType(argTypes: ArrayBuffer[Type],
                    returnType: Type) extends Type

// Type-Checking
case class UnknownType() extends Type