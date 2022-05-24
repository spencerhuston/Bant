package Lexer.SyntaxDefinitions

object Delimiters extends Enumeration {
  val COLON = Value(":")
  val SEMI_COLON = Value(";")
  val STRING_QUOTE = Value("\"")
  val CHAR_QUOTE = Value("\'")
  val COMMA = Value(",")
  val LEFT_PAREN = Value("(")
  val RIGHT_PAREN = Value(")")
  val LEFT_BRACKET = Value("[")
  val RIGHT_BRACKET = Value("]")
  val LEFT_BRACE = Value("{")
  val RIGHT_BRACE = Value("}")
  val RETURN_TYPE = Value("->")
  val ASSIGNMENT = Value("=")
  val COVARIANT = Value(":>")
  val BIRD = Value("|>")
}
