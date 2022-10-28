package Parser

import Lexer.SyntaxDefinitions.Delimiters
import Lexer.Token
import SemanticAnalyzer.{Type, UnknownType}

import scala.collection.mutable.ArrayBuffer

abstract class Exp {
  def token: Token
  var expType: Type = UnknownType()

  def usingType(tp: Type): Exp = {
    expType = tp
    this
  }
}

// Primitives
case class Lit(token: Token,
               value: Val) extends Exp

sealed trait Val { }
case class IntVal(value: Int) extends Val
case class BoolVal(value: Boolean) extends Val
case class CharVal(value: String) extends Val
case class StringVal(value: String) extends Val
case class NullVal() extends Val

// Pattern Matching Values
sealed trait CasePattern { }
case class TypeCase(ident: String,
                    caseType: Type) extends CasePattern
case class ValueCase(value: ValueCasePattern) extends CasePattern

sealed trait ValueCasePattern { }
case class ConstructorCase(ident: String,
                           values: ArrayBuffer[ValueCasePattern]) extends ValueCasePattern
case class LitCase(value: Val) extends ValueCasePattern
case class AnyCase() extends ValueCasePattern

// Pattern Matching
case class Match(token: Token,
                 value: Exp,
                 cases: ArrayBuffer[Case]) extends Exp
case class Case(token: Token,
                casePattern: CasePattern,
                caseExp: Exp) extends Exp

// Data & Record Types and Typeclasses
case class Alias(token: Token,
                 alias: String,
                 actualType: Type,
                 afterAlias: Exp) extends Exp
case class Constructor(members: ArrayBuffer[Type])
case class Adt(token: Token,
               ident: String,
               generics: ArrayBuffer[Generic],
               derivedFrom: Ref,
               constructors: ArrayBuffer[Constructor],
               afterAdt: Exp) extends Exp
case class Member(ident: String, memberType: Type)
case class Record(token: Token,
                  isSealed: Boolean,
                  ident: String,
                  generics: ArrayBuffer[Generic],
                  superType: Ref,
                  derivedFrom: Ref,
                  members: ArrayBuffer[Member],
                  afterRecord: Exp) extends Exp
case class Signature(name: Ref, funcType: Type)
case class Typeclass(token: Token,
                     isSealed: Boolean,
                     ident: String,
                     genericTypes: ArrayBuffer[Generic],
                     superclass: Ref,
                     signatures: ArrayBuffer[Signature],
                     afterTypeclass: Exp) extends Exp
case class Instance(token: Token,
                    adt: Ref,
                    typeclassIdent: Ref,
                    funcs: ArrayBuffer[FuncDef],
                    afterInstance: Exp) extends Exp

// Functions
case class Prog(token: Token,
                funcs: ArrayBuffer[FuncDef],
                afterProg: Exp) extends Exp
case class Generic(ident: String, lowerBound: String, upperBound: String)
case class Parameter(token: Token,
                     ident: String,
                     paramType: Type,
                     default: Exp) extends Exp
case class FuncDef(token: Token,
                   ident: String,
                   generics: ArrayBuffer[Generic],
                   params: ArrayBuffer[Parameter],
                   returnType: Type,
                   body: Exp) extends Exp

// Function Application/Data & Record Construction/Collection Access/
// FuncApp covers:
// - Function Application
// - ADT Construction
// - Collection Access
case class FuncApp(token: Token,
                   ident: Exp,
                   genericParameters: ArrayBuffer[Type],
                   arguments: ArrayBuffer[Exp]) extends Exp
case class TupleAccess(token: Token,
                       ref: Exp,
                       accessIndex: IntVal) extends Exp
case class RecordAccess(token: Token,
                        recordIdent: Ref,
                        fieldIdent: String) extends Exp

// General
case class Let(token: Token,
               isLazy: Boolean,
               ident: String,
               letType: Type,
               expValue: Exp,
               afterLet: Exp) extends Exp
case class Prim(token: Token,
                op: Delimiters.Value,
                left: Exp,
                right: Exp) extends Exp
case class Ref(token: Token,
               ident: String) extends Exp
case class Branch(token: Token,
                  condition: Exp,
                  ifBranch: Exp,
                  elseBranch: Exp) extends Exp

// Collections
case class ListDef(token: Token,
                   values: ArrayBuffer[Exp]) extends Exp
case class ArrayDef(token: Token,
                    values: ArrayBuffer[Exp]) extends Exp
case class SetDef(token: Token,
                  values: ArrayBuffer[Exp]) extends Exp
case class TupleDef(token: Token,
                    values: ArrayBuffer[Exp]) extends Exp
case class Map(key: Exp, value: Exp)
case class DictDef(token: Token,
                   mapping: ArrayBuffer[Map]) extends Exp

// None - Error occurred or EOF
case class NoOp(token: Token) extends Exp
