package Lexer

case class FilePosition(line: Int, column: Int, lineText: String)

sealed trait Token {
  def fp: FilePosition
  def tokenText: String
}

case class Terminator(
  val fp: FilePosition,
  val tokenText: String) extends Token
case class Delimiter(
  val fp: FilePosition,
  val tokenText: String) extends Token
case class Keyword(
  val fp: FilePosition,
  val tokenText: String) extends Token
case class Value(
  val fp: FilePosition,
  val tokenText: String) extends Token
case class Ident(
  val fp: FilePosition,
  val tokenText: String) extends Token