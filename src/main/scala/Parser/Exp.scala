package Parser

import Lexer.Token
import TypeChecker.{Type, UnknownType}

import scala.collection.mutable.ArrayBuffer

abstract class Exp {
  def token: Token
  var expType: Type = UnknownType()

  def usingType(tp: Type): Exp = {
    expType = tp
    this
  }
}

// General
case class Let(token: Token,
               isLazy: Boolean,
               ident: String,
               letType: Type,
               expValue: Exp,
               afterLet: Exp) extends Exp
case class Prim(token: Token,
                left: Exp,
                right: Exp) extends Exp
case class Ref(token: Token,
               ident: String) extends Exp
case class Branch(token: Token,
                  condition: Exp,
                  ifBranch: Exp,
                  elseBranch: Exp) extends Exp

// Primitives
case class Lit(token: Token,
               value: Val) extends Exp

sealed trait Val { }
case class IntVal(value: Int) extends Val
case class BoolVal(value: Boolean) extends Val
case class CharVal(value: String) extends Val
case class StringVal(value: String) extends Val
case class NullVal() extends Val

// Functions
case class Prog(token: Token,
                funcs: ArrayBuffer[FunDef]) extends Exp
case class FunDef(token: Token) extends Exp
case class Arg(token: Token) extends Exp
case class Lambda(token: Token,
                  args: ArrayBuffer[Arg],
                  body: Exp) extends Exp
case class App(token: Token) extends Exp

// ADT
case class Adt(token: Token) extends Exp
case class Typeclass(token: Token) extends Exp
case class Instance(token: Token) extends Exp

// Collections
case class ListDef(token: Token) extends Exp
case class ArrayDef(token: Token) extends Exp
case class TupleDef(token: Token) extends Exp
case class DictDef(token: Token) extends Exp
case class SetDef(token: Token) extends Exp
case class BlockGet(token: Token) extends Exp

// Pattern Matching
case class Match(token: Token,
                 value: Exp,
                 cases: ArrayBuffer[Case]) extends Exp
case class Case(token: Token,
                casePattern: CasePattern,
                caseExp: Exp) extends Exp

// Pattern Matching Values
sealed trait CasePattern { }
case class TypeCase(ident: String,
                    caseType: Type) extends CasePattern
case class ValueCase(value: ValueCasePattern) extends CasePattern

sealed trait ValueCasePattern { }
case class ConstructorCase(ident: String, values: ArrayBuffer[ValueCasePattern]) extends ValueCasePattern
case class LitCase(value: Val) extends ValueCasePattern
case class AnyCase() extends ValueCasePattern

case class NoOp(token: Token) extends Exp