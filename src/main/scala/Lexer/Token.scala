package Lexer

import Position.filePositionFactory
import SyntaxDefinitions.{Delimiters, Keywords}

case class FilePosition(line: Int, column: Int, lineText: String)

sealed trait Token {
  def tokenText: String
  def fp: FilePosition
}

case class Terminator(tokenText: String,
                      fp: FilePosition = filePositionFactory) extends Token
case class Delimiter(delim: Delimiters.Value,
                     tokenText: String,
                     fp: FilePosition = filePositionFactory) extends Token
case class Keyword(keyword: Keywords.Value,
                   tokenText: String,
                   fp: FilePosition = filePositionFactory) extends Token
case class Value(tokenText: String,
                 fp: FilePosition = filePositionFactory) extends Token
case class Ident(tokenText: String,
                 fp: FilePosition = filePositionFactory) extends Token
case class EOF(tokenText: String = "EOF",
               fp: FilePosition = filePositionFactory) extends Token