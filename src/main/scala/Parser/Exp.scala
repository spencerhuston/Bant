package Parser

import Lexer.Token

sealed trait Exp {
  def token: Token
}

// General
case class Let(token: Token) extends Exp
case class Lit(token: Token) extends Exp
case class Prim(token: Token) extends Exp
case class Ref(token: Token) extends Exp
case class Branch(token: Token) extends Exp

// Functions
case class Prog(token: Token) extends Exp
case class FunDef(token: Token) extends Exp
case class Arg(token: Token) extends Exp
case class Lambda(token: Token) extends Exp
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
case class Match(token: Token) extends Exp
case class Switch(token: Token) extends Exp
case class Case(token: Token) extends Exp