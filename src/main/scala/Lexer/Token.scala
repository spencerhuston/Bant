package Lexer

import Position.filePositionFactory

case class FilePosition(line: Int, column: Int, lineText: String)

sealed trait Token {
  def fp: FilePosition
  def tokenText: String
}

case class Terminator(
  val tokenText: String,
  val fp: FilePosition = filePositionFactory) extends Token
case class Delimiter(
  val tokenText: String,
  val fp: FilePosition = filePositionFactory) extends Token
case class Keyword(
  val tokenText: String,
  val fp: FilePosition = filePositionFactory) extends Token
case class Value(
  val tokenText: String,
  val fp: FilePosition = filePositionFactory) extends Token
case class Ident(
  val tokenText: String,
  val fp: FilePosition = filePositionFactory) extends Token
case class EOF(
  val tokenText: String = "EOF",
  val fp: FilePosition = filePositionFactory) extends Token